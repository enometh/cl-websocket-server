;;;; cl-websocket-server.asd
;;
;;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>


(asdf:defsystem #:websocket-server
  :description "simple websocket server framework."
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "gpl 2.0 or later"
  :version "0.0.1"
  :serial t
  :depends-on (:clack :websocket-driver)
  :components ((:file "package")
               (:file "websocket-server")))
