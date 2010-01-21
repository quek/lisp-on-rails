;;;; -*- Mode: LISP; -*-
(in-package #:cl-user)
(defpackage #:active-record-system (:use #:asdf #:cl))
(in-package #:active-record-system)

(defsystem :active-record
  :version "0.0.0"
  :components ((:module "active-record"
                        :serial t
                        :components ((:file "packages")
                                     (:file "active-record"))))
  :depends-on (clsql))

(defmethod perform ((o test-op) (c (eql (asdf:find-system :active-record))))
  (operate 'load-op :active-record-test)
  (operate 'test-op :active-record-test :force t))

(defsystem :active-record-test
  :components ((:module "t"
                        :serial t
                        :components ((:file "active-record-test"))))
  :depends-on (:active-record :stefil))


(defmethod perform ((o test-op) (c (eql (find-system :active-record-test))))
  (operate 'load-op :active-record-test)
  (if (funcall (intern (symbol-name :active-record-test)
		       (find-package :active-record-test)))
      (princ "ok")
      (error "test-op failed")))
