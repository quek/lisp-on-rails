;;;; -*- Mode: LISP; -*-
(in-package #:cl-user)
(defpackage #:lack-system (:use #:asdf #:cl))
(in-package #:lack-system)

(defsystem :lack
  :version "0.0.0"
  :components ((:module "lack"
                        :serial t
                        :components ((:file "packages")
                                     (:file "lack"))))
  :depends-on (anaphora hunchentoot))

(defmethod perform ((o test-op) (c (eql (asdf:find-system :lack))))
  (operate 'load-op :lack-test)
  (operate 'test-op :lack-test :force t))

(defsystem :lack-test
  :components ((:module "t"
                        :serial t
                        :components ((:file "lack-test"))))
  :depends-on (:lack :stefil))


(defmethod perform ((o test-op) (c (eql (find-system :lack-test))))
  (operate 'load-op :lack-test)
  (if (funcall (intern (symbol-name :lack-test)
		       (find-package :lack-test)))
      (princ "ok")
      (error "test-op failed")))
