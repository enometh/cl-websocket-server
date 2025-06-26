;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sat Jun 21 20:24:46 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; with ideas from rabbibotton/clog and byulparan/websocket-demo
;;; but without a parenscript layer.

(defpackage "MINI-CLOG"
  (:use "CL")
  (:import-from "WEBSOCKET-SERVER"
   "CALL-IN-WS-REPL"
   "WITH-BATCH-TRANSACTIONS"
   "ESCAPE-STRING"))
(in-package "MINI-CLOG")

;;; GENERAL  NOTES
;;;
;;; 1. no clos objects. dom elements are directly designated by "id
;;; strings" which are indexed in the `objreg' dictionary of the web
;;; page. `objreg' is initialized by calling `boot'.
;;;
;;; 2. `create-*' functions take a mandatory (sic) &optional id-var in
;;; the lambda list which helps the programmer name the id strings
;;; directly.  This leads to idiosyncratic non-standard &optional &key
;;; signatures which may annoy in the programmer in a non-experimental
;;; setting.
;;;
;;; 3. no generic functions or methods. so API functions have to use
;;; prefixes to differentiate (e.g. rotate vs. matrix-rotate).

;;; ad-hoc hooks system
(defun add-to-hook (hook-var sym &optional prepend)
  (check-type sym symbol)
  (assert (fboundp sym))
  (unless (find sym (symbol-value hook-var))
    (set hook-var
	 (if prepend
	     (cons sym (symbol-value hook-var))
	     (nconc (symbol-value hook-var) (list sym))))
  hook-var))

(defun run-hooks (hook-var)
  (loop for sym in (symbol-value hook-var)
	do (with-simple-restart (cont "Cont")
	     (assert (fboundp sym))
	     (funcall sym))))

(defun remove-hook (hook-var sym)
  (set hook-var (delete sym (symbol-value hook-var))))

(defvar *boot-hooks* nil)

(defun boot ()
  "Initializes the `objreg' dictionary object on the default ws
connection."
  (call-in-ws-repl "var boot = true;
try {
  if (!(objreg === undefined)) {
    boot = false;
  }
} catch (error) {
}
if (boot) { objreg = {} }
check_id = function (id) {
  if (!(undefined === objreg[id])) {
    throw `${id} already assigned to ${objreg[id].constructor.name}`
  }
}
document.body.id= 'body'; objreg['body']=document.body")
  (run-hooks '*boot-hooks*))

(defun by-id (id)
  (call-in-ws-repl (format nil "document.getElementById('~A')" id)))

(defun objreg (id)
  (format nil "objreg['~A']" id))

(defun get-objreg (id)
  ;; "defined for illustration-al purposes only"
  (call-in-ws-repl (objreg id)))

(defun delete-element (child-id)
  "Remove an htmlElement with id CHILD-ID from the document tree. (This
would have been created with CREATE-ELEMENT and added to the document
tree with APPEND-CHILD.)"
  (call-in-ws-repl (format nil "var a = objreg['~A']; a.parentNode.removeChild(a);" child-id)))

(defvar *new-id* (list 0))
(defun generate-id ()
  (incf (car *new-id*)))

(defmacro with-id-var ((id-var) &body body)
  `(let ((,id-var (or ,id-var (format nil "G~A-~A" ',id-var (generate-id)))))
     ,@body))

(defun method-call (obj methodname &rest args)
  (call-in-ws-repl (format nil "objreg['~A'].~A(~{~A~^,~})" obj methodname args)))

(defun create-element (tagname &optional html-id)
  "Creates and registers an htmlElement of type TAGNAME.  Does not
add it to the document's children."
  (with-id-var (html-id)
    (call-in-ws-repl (format nil "
var html_id = '~A';
var tagname = '~A';
check_id(html_id);
objreg[html_id] = document.createElement(tagname);
objreg[html_id].id = html_id;
html_id"
			     html-id tagname))))

(defun get-attr (html-id attr &rest more-attrs)
  (call-in-ws-repl (format nil "objreg['~A'].getAttribute('~A')~{?.getAttribute('~A')~}" html-id attr more-attrs)))

(defsetf get-attr (html-id attr &rest more-attrs) (new-value)
  (let* ((all-attrs (cons attr more-attrs))
	 (tail-cons (last all-attrs))
	 (pre-attrs (ldiff all-attrs tail-cons)))
    `(progn (call-in-ws-repl (format nil "objreg['~A']~{.?getAttribute('~A')~}.setAttribute('~A','~A')" ,html-id ,pre-attrs ,(car tail-cons) ,new-value))
	    ,new-value)))

(defun get-prop (html-id prop &rest more-props)
  (call-in-ws-repl (format nil "objreg['~A'].~A~{?.~A~}" html-id prop more-props)))

(defsetf get-prop (html-id prop &rest more-props) (new-value)
  (let* ((all-props (cons prop more-props))
	 (tail-cons (last all-props))
	 (pre-props (ldiff all-props tail-cons)))
    `(progn (call-in-ws-repl (format nil "objreg['~A']~{.~A~}.~A= ~A" ,html-id ',pre-props ,(car tail-cons) ,new-value))
	    ,new-value)))

(defun append-child (child-id &optional parent-html-id)
  (call-in-ws-repl (format nil "~A.appendChild(objreg['~A']);"
			   (if parent-html-id
			       (format nil "objreg['~A']" parent-html-id)
			       "document.body")
			   child-id)))

(defun fmt-ws (fmt-string &rest fmt-args)
  (call-in-ws-repl (apply #'format nil fmt-string fmt-args)))

(defun insert-before (child-id before-id &optional parent-html-id)
  (fmt-ws "~A.insertBefore(objreg['~A'],objreg['~A']);"
	   (if parent-html-id
	       (format nil "objreg['~A']" parent-html-id)
	       (format nil "objreg['~A'].parentNode"
		       before-id))
	   child-id
	   before-id))

#||
(boot)
(call-in-ws-repl "document.body.childElementCount")
(call-in-ws-repl "document.body.removeChild(document.body.children[document.body.childElementCount - 1])")
(setq $p (create-element "p"))
(append-child $p)
(setq $txt "   London. Michaelmas term lately over, and the Lord Chancellor sitting in
    Lincoln's Inn Hall. Implacable November weather. As much mud in the streets
    as if the waters had but newly retired from the face of the earth, and it
    would not be wonderful to meet a Megalosaurus, forty feet long or so,
    waddling like an elephantine lizard up Holborn Hill.")
(setf (get-prop $p "innerHTML")
      (concatenate 'string "'" (escape-string $txt) "'"))
websocket-server:*client*
(setq $s (create-element "script"))
;; define a function `ty' on the webpage and call it
(append-child $s)
(get-prop $s "innerHTML")
(setf (get-prop $s "innerHTML")
      "function ty(c) {return c.constructor.name}")
(call-in-ws-repl (format nil "ty(objreg['~a'])" $s))
(delete-element $s)
(setf (get-attr $s "src")
      "/home/madhu/public_html/stage/webgl-drexel/greggman.github.io/webgl-lint/webgl-lint.js")
(setf (get-attr $s "src") "")
(setf (get-attr $s "src")  "/dev/shm/f1.js")
(append-child $s)
(call-in-ws-repl "Object.getOwnPropertyDescriptors(objreg)")
||#
