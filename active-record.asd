;;;; -*- Mode: LISP; -*-
(asdf:defsystem :active-record
  :version "0.0.0"
  :serial t
  :components ((:module "active-record"
                        :components ((:file "packages")
                                     (:file "active-record"))))
  :depends-on (clsql))

