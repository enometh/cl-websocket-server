;;;; package.lisp
;;
;;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>


(defpackage #:websocket-server
  (:use #:cl)
  (:export
   #:chat-server
   #:broadcast-message
   "QUERY"))
