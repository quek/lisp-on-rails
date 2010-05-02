;;;; -*- Mode: LISP; -*-
(defsystem :blog
  :version "0.0.0"
  :serial t
  :components ((:file "packages")
               (:file "blog")
               (:file "routes"))
  :depends-on (:action-pack :active-record))
