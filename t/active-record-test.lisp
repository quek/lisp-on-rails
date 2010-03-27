(defpackage active-record-test
    (:use :cl :active-support :active-record :stefil))

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
        ;;(describe b)
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
               (setf (post-of c) post)
               (save c))))
  (let* ((post (car (all post)))
         (comments (comments-of post)))
    ;;(mapc #'describe comments)
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
    (setf (post-of post-info) post)
    (save post-info))
  (let* ((post (car (all post)))
         (post-info (post-info-of post)))
    (is (string= "まみむめも♪" (info-of post-info)))))

(deftest test-update ()
  (let ((post (save (make-instance 'post :name "名前"
                                   :title "タイトル"
                                   :content "内容"))))
    (setf (name-of post) "更新")
    (save post)
    (let ((post (select 'post (id-of post))))
      (is (string= "更新" (name-of post))))))

(deftest test-select ()
  (mapc #'destroy (all post))
  (let* ((posts (loop for i from 1 to 10
                      collect (save (make-instance post
                                         :name (str i)
                                         :title (str "タイトル" i)
                                         :content (str "内容" i)))))
         (cadr (cadr posts)))
    (is (string= (title-of cadr) (title-of (select post (id-of cadr)))))
    (is (= 3 (length (select post (loop repeat 3 for i in posts
                                        collect (id-of i))))))
    (is (= 10 (length (select post :all))))
    (is (= 1 (length (select post :all :conditions '(:name "2")))))
    (is (= 1 (length (select post :all
                             :conditions '(:name "2" :title "タイトル2")))))
    (is (= 0 (length (select post :all
                             :conditions '(:name "3" :title "タイトル2")))))
    (is (= 3 (length (select post :all
                             :conditions '(:name ("1" "3" "5"))))))
    (is (string= "タイトル1" (title-of (select post :first))))
    (is (string= "タイトル10" (title-of (select post :last))))
    (is (string= "2" (name-of (select post (id-of cadr)
                                      :conditions '(:name "2")))))))

;;(active-record-test)