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
(defparameter *ws* nil)
(defparameter *connections* (make-hash-table))

;; and assign a random nickname to a user upon connection
(defun handle-new-connection (con)
  (setf (gethash con *connections*)
        (format nil "user-~a" (random 100000))))

(defun broadcast-to-room (connection message)
  (let ((message (format nil "~a: ~a"
                         (gethash connection *connections*)
                         message)))
    (loop :for con :being :the :hash-key :of *connections* :do
          (websocket-driver:send con message))))

(defun handle-close-connection (connection)
  (let ((message (format nil " .... ~a has left."
                         (gethash connection *connections*))))
    (remhash connection *connections*)
    (loop :for con :being :the :hash-key :of *connections* :do
          (websocket-driver:send con message))))

(defun chat-server (env)
  (let ((ws (websocket-driver:make-server env)))
    (setf *ws* ws)
    (websocket-driver:on :open ws
                         (lambda () (handle-new-connection ws)))
    (websocket-driver:on :message ws
                         (lambda (msg)
                           (broadcast-to-room ws msg)
                           (let* ((form (read-from-string msg))
                                  (fn (if (listp form)
                                          (symbol-function
                                           (intern (string-upcase (format nil "~a" (first form))) 'websocket-server)))))
                             (and fn (apply fn (rest form))))))
    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (handle-close-connection ws)))
    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws)))) ; send the handshake

(defparameter *msg* nil)

;; keep the handler around so that you can stop your server later on

(defparameter *chat-handler* nil)

;;; (setf *chat-handler* (clack:clackup #'chat-server :port 12345))

;;; (hunchentoot::shutdown)

;;; (clack:stop *chat-handler*)

(defun broadcast-message (msg)
  (loop :for con :being :the :hash-key :of *connections* :do
    (websocket-driver:send con msg)))
