;;;; -*- Mode: LISP; -*-
(defsystem :blog
  :version "0.0.0"
  :serial t
  :components ((:module "../config"
                        :serial t
                        :components ((:file "packages")
                                     (:file "environment")
                                     (:file "routes")))
               (:module "../app/models"
                        :serial t
                        :components ())
               (:module "../app/controllers"
                        :serial t
                        :components ((:file "application-controller")
                                     (:file "top-controller")))
               (:module "../app/helpers"
                        :serial t
                        :components ((:file "application-helper")))
               (:module "../app/views"
                        :serial t
                        :components ()))
  :depends-on (:railties :action-pack :active-record))
