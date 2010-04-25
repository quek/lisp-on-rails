(in-package :lack-hunchentoot)

(setf hunchentoot:*hunchentoot-default-external-format*
      (flexi-streams:make-external-format :utf-8)
      hunchentoot:*default-content-type* "text/html; charset=utf-8"
      hunchentoot:*show-lisp-errors-p* t)

(defvar *env*)

(defclass hunchentoot-web-server (web-server) ())

(defmethod run ((web-server hunchentoot-web-server) x)
  (labels ((dispatch ()
             (destructuring-bind (status header body)
                 (call x (make-env))
               (finish-response status header body))))
    (push (hunchentoot:create-prefix-dispatcher *url-prefix* #'dispatch)
          hunchentoot:*dispatch-table*)
    (let ((acceptor (make-instance 'hunchentoot:acceptor :port *port*)))
      (hunchentoot:start acceptor))))

(defun make-env ()
  )

(defun finish-response (status header body)
  (with-output-to-string (out)
    (loop for i in body
       do (princ i out))))

#|
(progn
  (defclass foo () ())
  (defmethod call ((foo foo) env)
    `(200 () ("<h1>" "Hello " ,(get-universal-time) "</h1>")))
  (lack:run (make-instance 'lack-hunchentoot:hunchentoot-web-server)
            (make-instance 'lack-hunchentoot::foo)))
|#
