;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sat Jun 14 23:27:17 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
(in-package "WEBSOCKET-SERVER")

;; a munged implementation from that in clozure common lisp.

(defstruct id-map
  (lock (bordeaux-threads:make-lock "id-map-lock"))
  (queue (make-array 0 :adjustable t :fill-pointer t))
  (free nil))

(defun id-map-add (id-map obj)
  (check-type obj (not (or null fixnum)))
  (with-slots (free lock queue) id-map
    (let (idx)
      (bordeaux-threads:with-lock-held (lock)
	(cond ((setq idx free)
	       (setq free (elt queue idx))
	       (setf (elt queue idx) obj))
	      (t (setq idx (length queue))
		 (assert (= idx (vector-push-extend obj queue))))))
      idx)))

(defun id-map-peek (id-map id)
 (with-slots (lock queue) id-map
  (let ((obj (bordeaux-threads:with-lock-held (lock)
	       (elt queue id))))
    (check-type obj (not (or null fixnum)))
    obj)))

(defun id-map-remove (id-map id)
  (with-slots (lock queue free) id-map
    (bordeaux-threads:with-lock-held (lock)
      (let ((obj (elt queue id)))
	(check-type obj (not (or null fixnum)))
	(setf (elt queue id) free)
	(setf free id)
	obj))))


#||
(defvar *id-map* (make-id-map))
(id-map-add *id-map* (make-mailbox))
(id-map-remove *id-map* 1)
(id-map-remove *id-map* 2)
(id-map-remove *id-map* 0)
||#

;; from clog
(defun escape-string (str &key (no-nil nil) (html nil))
  "Escape STR for sending to browser script. If no-nil is t (default is nil)
if str is NIL returns empty string otherwise returns nil. If html is t the
quotes are changed to html entities and \n and \r are eliminated. Escape
string is used for wire readiness i.e. ability to be evaluated client side
and not for security purposes or html escapes."
  (if (and (not str) (not no-nil))
      nil
      (let ((res))
        (setf res (format nil "~@[~A~]" str))
        (setf res (ppcre:regex-replace-all "\\x5C" res "\\x5C")) ; \
        (cond (html
               (setf res (ppcre:regex-replace-all "\\x22" res "&#x22;")) ; "
               (setf res (ppcre:regex-replace-all "\\x27" res "&#x27;")) ; '
               (setf res (ppcre:regex-replace-all "\\x0A" res "&#x0A;")) ; \n
               (setf res (ppcre:regex-replace-all "\\x0D" res "&#x0D"))) ; \r
              (t
               (setf res (ppcre:regex-replace-all "\\x22" res "\\x22")) ; "
               (setf res (ppcre:regex-replace-all "\\x27" res "\\x27")) ; '
               (setf res (ppcre:regex-replace-all "\\x0A" res "\\x0A")) ; \n
               (setf res (ppcre:regex-replace-all "\\x0D" res "\\x0D")))) ; \r
        res)))
