(defpackage active-record-test
    (:use :cl :active-record :stefil))

(in-package :active-record-test)

(defsuite active-record-test)
(in-suite active-record-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *connection-spec* '("localhost" "blog_test" "root" ""))
  (setq clsql-sys:*default-database-type* :mysql)
  (establish-connection))

(def-record post)

(deftest test-all ()
  (mapc #'destroy (all post))
  (let ((a (make-instance 'post :name "名前" :title "タイトル" :content "内容")))
    (save a)
    (let ((all (all post)))
      (is (= 1 (length all)))
      (let ((b (car all)))
        (is (string= "名前" (name-of b)))
        (is (string= "タイトル" (title-of b)))
        (is (string= "内容" (content-of b)))))))



;;(active-record-test)