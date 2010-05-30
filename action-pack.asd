;;;; -*- Mode: LISP; -*-
(in-package #:cl-user)
(defpackage #:action-pack-system (:use #:asdf #:cl))
(in-package #:action-pack-system)

(defsystem :action-pack
  :version "0.0.0"
  :components ((:module "action-pack"
                        :serial t
                        :components ((:file "packages")
                                     (:file "vars")
                                     (:file "ecl")
                                     (:file "dispatcher")
                                     (:file "routes")
                                     (:file "action-controller"))))
  :depends-on (lack active-support))

(defmethod perform ((o test-op) (c (eql (asdf:find-system :action-pack))))
  (operate 'load-op :action-pack-test)
  (operate 'test-op :action-pack-test :force t))

(defsystem :action-pack-test
  :components ((:module "t"
                        :serial t
                        :components ((:file "action-pack-test"))))
  :depends-on (:action-pack :stefil))


(defmethod perform ((o test-op) (c (eql (find-system :action-pack-test))))
  (operate 'load-op :action-pack-test)
  (if (funcall (intern (symbol-name :action-pack-test)
		       (find-package :action-pack-test)))
      (princ "ok")
      (error "test-op failed")))
