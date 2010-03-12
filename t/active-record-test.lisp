(defpackage active-record-test
    (:use :cl :active-record :stefil))

(in-package :active-record-test)

(defsuite active-record-test)
(in-suite active-record-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *connection-spec* '("localhost" "blog_test" "root" ""))
  (setq clsql-sys:*default-database-type* :mysql)
  (establish-connection))

#|
show create table comments;
CREATE TABLE `comments` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `commenter` varchar(255),
  `body` text,
  `post_id` int(11),
  `created_at` datetime,
  `updated_at` datetime,
  PRIMARY KEY (`id`)
);
CREATE TABLE `posts` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255),
  `title` varchar(255),
  `content` text,
  `created_at` datetime,
  `updated_at` datetime,
  PRIMARY KEY (`id`)
);
CREATE TABLE `post_infos` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `post_id` int(11),
  `info` varchar(255),
  `created_at` datetime,
  `updated_at` datetime,
  PRIMARY KEY (`id`)
);
|#
(def-record comment
  (:belongs-to post))
(def-record post
  (:has-many comments)
  (:has-one post-info))
(def-record post-info
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

(deftest test-has-one ()
  (mapc #'destroy (append (all post) (all comment) (all post-info)))
  (let ((post (save (make-instance 'post :name "名前"
                                   :title "タイトル"
                                   :content "内容")))
        (post-info (make-instance 'post-info :info "まみむめも♪")))
    (setf (post-id-of post-info) (id-of post))
    (save post-info))
  (let* ((post (car (all post)))
         (post-info (post-info-of post)))
    (is (string= "まみむめも♪" (info-of post-info)))))
;;(active-record-test)
