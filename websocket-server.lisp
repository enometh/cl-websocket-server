;;;
;;; websocket-server.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :websocket-server)

;; make a hash table to map connections to nicknames
(defvar *ws* nil)
(defvar *connections* (make-hash-table))

;; and assign a random nickname to a user upon connection
(defun handle-new-connection (con)
  (setf (gethash con *connections*)
        (format nil "user-~a" (random 100000))))

(defun broadcast-to-room (connection message)
  (let ((message (format nil "'~a: ~a'"
                         (gethash connection *connections*)
                         message)))
    (loop :for con :being :the :hash-key :of *connections* :do
          (websocket-driver:send con message))))

(defun handle-close-connection (connection)
  (let ((message (format nil "' .... ~a has left.'"
                         (gethash connection *connections*))))
    (remhash connection *connections*)
    (when (eql *client* connection) (setq *client* nil))
    (loop :for con :being :the :hash-key :of *connections* :do
          (websocket-driver:send con message))))

(defun handle-chat-msg (ws msg)
  (broadcast-to-room ws msg)
  (let* ((form (read-from-string msg))
	 (fn (if (listp form)
		 (symbol-function
		  (find-symbol (string-upcase (format nil "~a" (first form))) 'websocket-server)))))
    (and fn (apply fn (rest form)))))

(defvar *id-map* (make-id-map))

(defvar *query-time-out* 5)

(defstruct mailbox result semaphore errorp default-answer)

(define-condition websocket-repl-error (error)
  ((error-report :initarg :error-report :reader error-report))
  (:report (lambda (condition stream)
	     (format stream "~a" (error-report condition)))))

(defun query (connection script &optional default-answer)
  (let* ((mbox (make-mailbox :semaphore (bt:make-semaphore)
			    :default-answer default-answer))
	 (id (id-map-add *id-map* mbox)))
    (websocket-driver:send
     connection
     (format nil "var result = null;
var id = \"~A\";
var numberp = 'NIL';
var errorp = 'NIL';
try {
  result = eval(\"~A\");
  if (typeof result === 'number') {
    numberp = ':NUMBER';
  } else if (typeof result === 'boolean') {
    numberp = ':BOOLEAN';
  }
} catch (error) {
  result = error;
  numberp = 'NIL';
  errorp = 'T';
} finally {
  socket.send('(' + id + ' ' + numberp + ' ' + errorp + ')' + result);
};" id (escape-string script)))
    (unwind-protect
	 (progn (bt:wait-on-semaphore (mailbox-semaphore mbox) :timeout *query-time-out*)
		(if (mailbox-errorp mbox)
		    (error 'websocket-repl-error :error-report (mailbox-result mbox))
		    (or (mailbox-result mbox)
			(mailbox-default-answer mbox))))
      (with-simple-restart (cont "CONT")
	(id-map-remove *id-map* id)))))

(defvar *client* nil)

(defun client-or-default ()
  (let ((client
	 (or *client* (car (cl-user::hash-keys
			    websocket-server::*connections*)))))
    (check-type client WEBSOCKET-DRIVER.WS.SERVER:SERVER)
    client))

(defun execute (thunk)
  (websocket-driver:send (client-or-default) thunk))


;;; ----------------------------------------------------------------------
;;;
;;; batch-transactions:  batch calls to call-in-ws-repl
;;;

(defvar *batch-transactions* nil)
(defvar *batch-transactions-dry-run-p* nil)

(defun begin-batch-transactions ()
  (assert (null *batch-transactions*))
  (setq *batch-transactions* (make-string-output-stream)))

(defun end-batch-transactions ()
  (assert (streamp *batch-transactions*))
  (let ((code (get-output-stream-string *batch-transactions*)))
    (cond (*batch-transactions-dry-run-p* code)
	  (t (setq *batch-transactions* nil)
	     (call-in-ws-repl code)))))

;; rly with-botched-transactions
(defmacro with-batch-transactions ((&key dry-run-p) &body body)
  `(let ((*batch-transactions-dry-run-p* ,dry-run-p)
	 ;;rebind to protect global value against errrors in body
	 (*batch-transactions* nil))
     (progn (begin-batch-transactions)
	    ,@body
	    (end-batch-transactions))))

(defun ends-with-semicolon (string)
  (loop for i from (1- (length string)) downto 0
	for c = (char string i)
	do (cond ((find c #(#\Newline #\Space #\Tab)) t)
		 ((eql c #\;) (return t))
		 (t (return nil)))))

(defun collect-batch-transactions (string)
  (write-string string *batch-transactions*)
  (unless (ends-with-semicolon string)
    (write-char #\; *batch-transactions*)
    (terpri *batch-transactions*))
  :invalid)



;;; ----------------------------------------------------------------------
;;;
;;; if *batch-transactions* is non-NIL collect STRING thunk in it,
;;; otherwise execute thunk.

(defun call-in-ws-repl (thunk)
  (if *batch-transactions*
      (collect-batch-transactions thunk)
      (let ((*client* (or *client* (car (cl-user::hash-keys
					 websocket-server::*connections*)))))
	(check-type *client* WEBSOCKET-DRIVER.WS.SERVER:SERVER)
	(query *client*  thunk))))

(defun handle-js-query (msg)
  (let* ((*read-eval* nil))
    (multiple-value-bind (elts offset)
	(read-from-string msg)
      (destructuring-bind (id numberp errorp) elts
	(let ((mbox (with-simple-restart (cont "CONT") (id-map-peek *id-map* id))))
	  (unless mbox (return-from handle-js-query :FAIL))
	  (setf (mailbox-errorp mbox) errorp)
	  (setf (mailbox-result mbox)
		(ecase numberp
		  (:number (parse-number:parse-number msg :start offset))
		  (:boolean (let ((ret (subseq msg offset)))
			      (cond ((equal ret "true") t)
				    ((equal ret "false") nil)
				    (t (error "Sanity")))))
		  ((nil)  (subseq msg offset))))
	  (bt:signal-semaphore (mailbox-semaphore mbox)))))))

(defun handle-message (ws msg)
  (cond ((equal msg "Heartbeat")
	 (websocket-driver:send ws "'Pong'"))
	((user:prefixp "eval:" msg)
	 (handle-chat-msg ws (subseq msg 5)))
	(t (handle-js-query msg))))

(defun chat-server (env)
  (let ((ws (websocket-driver:make-server env)))
    (setf *ws* ws)
    (websocket-driver:on :open ws
                         (lambda () (handle-new-connection ws)))
    (websocket-driver:on :message ws
                         (lambda (msg) (handle-message ws msg)))
    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (handle-close-connection ws)))
    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws)))) ; send the handshake

;; keep the handler around so that you can stop your server later on

(defvar *chat-handler* nil)

;;; (setq *chat-handler* (clack:clackup #'chat-server :port 12345))
;;; (clack:stop *chat-handler*)

(defun broadcast-message (msg)
  (loop :for con :being :the :hash-key :of *connections* :do
    (websocket-driver:send con msg)))
