(defpackage active-record-test
    (:use :cl :active-record :stefil))

(in-package :active-record-test)

(defsuite active-record-test)
(in-suite active-record-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *connection-spec* '("localhost" "blog_test" "root" ""))
  (setq clsql-sys:*default-database-type* :mysql)
  (establish-connection))

(def-record post
    (:has-many comments))
(def-record comment
    (:belongs-to post))

(deftest test-all ()
  (mapc #'destroy (all post))
  (let ((a (make-instance 'post :name "名前" :title "タイトル" :content "内容")))
    (save a)
    (is (id-of a))
    (let ((all (all post)))
      (is (= 1 (length all)))
      (let ((b (car all)))
        (describe b)
        (is (string= "名前" (name-of b)))
        (is (string= "タイトル" (title-of b)))
        (is (string= "内容" (content-of b)))))))

(deftest test-belogs-to ()
  (mapc #'destroy (append (all post) (all comment)))
  (let ((p1 (save (make-instance 'post :name "名前"
                                 :title "タイトル"
                                 :content "内容"))))
    (let ((c1 (make-instance 'comment :commenter "quek" :body "コメント")))
      (setf (post-of c1) p1)
      (is (eql (post-id-of c1) (id-of p1)))
      (save c1)))
  (let* ((c1 (car (all comment)))
         (p1 (post-of c1)))
    (is (string= "名前" (name-of p1)))
    (is (string= "タイトル" (title-of p1)))
    (is (string= "内容" (content-of p1)))))


;;(active-record-test)