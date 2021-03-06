(in-package :railties)

(defvar cl-user::*rails-env* :development)

(defun initialize ()
  (load-environment)
  (initialize-database))

(defun load-environment ()
  (load (environment-path)))

(defun environment-path ()
  (merge-pathnames
   (format nil "config/environments/~a.lisp"
           (string-downcase
            (symbol-name cl-user::*rails-env*)))
   cl-user::*rails-root*))

(defun initialize-database ()
  (let* ((specs
          (with-open-file (in (merge-pathnames "config/database.lisp"
                                               cl-user::*rails-root*))
            (read in)))
         (spec (cdr (find cl-user::*rails-env* specs :key #'car))))
    (setf active-record:*connection-spec* (getf spec :connection-spec)
          clsql-sys:*default-database-type* (getf spec :database-type))
    (active-record:establish-connection)))


(defmethod action-controller::dispatch (dispatcher &rest args)
   (clsql-sys:with-database (clsql-sys:*default-database*
                             active-record::*connection-spec*
                             :make-default t
                             :pool t
                             :encoding :utf-8)
     ;; TODO ↓ どっかちゃんとした場所で  ↑ もどうかな？
     (mapc #'clsql-sys:execute-command
           '("set character_set_client='utf8'"
             "set character_set_connection='utf8'"
             "set character_set_results='utf8'"))
     (call-next-method)))
