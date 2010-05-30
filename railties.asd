;;;; -*- Mode: LISP; -*-
(in-package #:cl-user)
(defpackage #:railties-system (:use #:asdf #:cl))
(in-package #:railties-system)

(defsystem :railties
  :version "0.0.0"
  :components ((:module "railties"
                        :serial t
                        :components ((:file "packages")
                                     (:file "initialize"))))
  :depends-on (active-support action-pack active-record))

(defmethod perform ((o test-op) (c (eql (asdf:find-system :railties))))
  (operate 'load-op :railties-test)
  (operate 'test-op :railties-test :force t))

(defsystem :railties-test
  :components ((:module "t"
                        :serial t
                        :components ((:file "railties-test"))))
  :depends-on (:railties :stefil))


(defmethod perform ((o test-op) (c (eql (find-system :railties-test))))
  (operate 'load-op :railties-test)
  (if (funcall (intern (symbol-name :railties-test)
		       (find-package :railties-test)))
      (princ "ok")
      (error "test-op failed")))
