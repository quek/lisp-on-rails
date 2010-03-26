;;;; -*- Mode: LISP; -*-
(in-package #:cl-user)
(defpackage #:active-support-system (:use #:asdf #:cl))
(in-package #:active-support-system)

(defsystem :active-support
  :version "0.0.0"
  :components ((:module "active-support"
                        :serial t
                        :components ((:file "packages")
                                     (:file "active-support"))))
  :depends-on (clsql anaphora closer-mop cl-ppcre))

(defmethod perform ((o test-op) (c (eql (asdf:find-system :active-support))))
  (operate 'load-op :active-support-test)
  (operate 'test-op :active-support-test :force t))

(defsystem :active-support-test
  :components ((:module "t"
                        :serial t
                        :components ((:file "active-support-test"))))
  :depends-on (:active-support :stefil))


(defmethod perform ((o test-op) (c (eql (find-system :active-support-test))))
  (operate 'load-op :active-support-test)
  (if (funcall (intern (symbol-name :active-support-test)
		       (find-package :active-support-test)))
      (princ "ok")
      (error "test-op failed")))
