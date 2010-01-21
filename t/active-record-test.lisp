(defpackage active-record-test
    (:use :cl :active-record :stefil))

(in-package :active-record-test)

(defsuite active-record-test)
(in-suite active-record-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *connection-spec* '("localhost" "blog_development" "root" ""))
  (setq clsql-sys:*default-database-type* :mysql)
  (establish-connection))

(def-record post)

(deftest test-all ()
  (is (posts-all)))

