(defparameter cl-user::*rails-root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(format t "*rails-root* => ~a~&" *rails-root*)

(flet ((f (path)
         (loop for path in (directory
                             (merge-pathnames path cl-user::*rails-root*))
               do (let ((pd (pathname-directory path)))
                    (unless (member "_darcs" pd :test #'equal)
                      (pushnew (make-pathname :directory pd)
                               asdf:*central-registry*
                               :test #'equal))))))
  (f "vendor/**/*.asd"))

(pushnew *load-pathname* asdf:*central-registry* :test #'equal)

(require :blog)
