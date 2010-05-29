(in-package :railties)

(defvar cl-user::*rails-env* :development)

(defun initialize ()
  (load-environment))

(defun load-environment ()
  (load (environment-path)))

(defun environment-path ()
  (merge-pathnames
   (format nil "config/environments/~a.lisp"
           (string-downcase
            (symbol-name cl-user::*rails-env*)))
   cl-user::*rails-root*))