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

(defstruct mailbox result semaphore default-answer)

(defun query (connection script &optional default-answer)
  (let* ((mbox (make-mailbox :semaphore (bt:make-semaphore)
			    :default-answer default-answer))
	 (id (id-map-add *id-map* mbox)))
    (websocket-driver:send
     connection
     (format nil "socket.send (\"~A:\"+eval(\"~A\"));"
	     id
	     (escape-string script)))
    (unwind-protect
	 (progn (bt:wait-on-semaphore (mailbox-semaphore mbox) :timeout *query-time-out*)
		(or (mailbox-result mbox)
		    (mailbox-default-answer mbox)))
      (id-map-remove *id-map* id))))

(defun handle-message (ws msg)
  (cond ((equal msg "Heartbeat")
	 (websocket-driver:send ws "'Pong'"))
	((user:prefixp "eval:" msg)
	 (handle-chat-msg ws (subseq msg 5)))
	(t	;javascript execution result
	 (let* ((p (position #\: msg))
		(mbox (id-map-peek *id-map* (parse-integer msg :end p))))
	   (setf (mailbox-result mbox) (subseq msg (1+ p)))
	   (bt:signal-semaphore (mailbox-semaphore mbox))))))

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
