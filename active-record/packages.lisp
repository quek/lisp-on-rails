(defpackage :active-record
    (:use :common-lisp)
  (:export #:*connection-spec*
           #:establish-connection
           #:base
           #:def-record))
