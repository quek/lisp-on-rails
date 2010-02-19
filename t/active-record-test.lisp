(defpackage active-record-test
    (:use :cl :active-record :stefil))

(in-package :active-record-test)

(defsuite active-record-test)
(in-suite active-record-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *connection-spec* '("localhost" "blog_test" "root" ""))
  (setq clsql-sys:*default-database-type* :mysql)
  (establish-connection))

(def-record comment
    (:belongs-to post))
(def-record post
    (:has-many comments))

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

(deftest test-has-many ()
  (mapc #'destroy (append (all post) (all comment)))
  (let ((post (save (make-instance 'post :name "名前"
                                   :title "タイトル"
                                   :content "内容"))))
    (loop for i from 1 to 3
          for c = (make-instance 'comment
                       :commenter(str "quek" i)
                       :body (str "コメント" i))
          do (progn
               (setf (post-id-of c) (id-of post))
               (save c))))
  (let* ((post (car (all post)))
         (comments (comments-of post)))
    (mapc #'describe comments)
    (is (= 3 (length comments)))
    (loop for i from 1 to 3
          for comment in comments
          do (progn
               (is (string= (str "quek" i) (commenter-of comment)))
               (is (string= (str "コメント" i) (body-of comment)))))))

;;(active-record-test)