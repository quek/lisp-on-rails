;;;; -*- Mode: LISP; -*-
(in-package #:cl-user)
(defpackage #:lack-hunchentoot-system (:use #:asdf #:cl))
(in-package #:lack-hunchentoot-system)

(defsystem :lack-hunchentoot
  :version "0.0.0"
  :components ((:module "lack/hunchentoot"
                        :serial t
                        :components ((:file "packages")
                                     (:file "lack-hunchentoot"))))
  :depends-on (lack alexandria))

(defmethod perform ((o test-op) (c (eql (asdf:find-system :lack-hunchentoot))))
  (operate 'load-op :lack-hunchentoot-test)
  (operate 'test-op :lack-hunchentoot-test :force t))

(defsystem :lack-hunchentoot-test
  :components ((:module "t"
                        :serial t
                        :components ((:file "lack-hunchentoot-test"))))
  :depends-on (:lack-hunchentoot :stefil))


(defmethod perform ((o test-op) (c (eql (find-system :lack-hunchentoot-test))))
  (operate 'load-op :lack-hunchentoot-test)
  (if (funcall (intern (symbol-name :lack-hunchentoot-test)
		       (find-package :lack-hunchentoot-test)))
      (princ "ok")
      (error "test-op failed")))
