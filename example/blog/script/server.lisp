(load (merge-pathnames "../config/boot.lisp" *load-truename*))

(require :lack-hunchentoot)

(defclass foo () ())

(defmethod lack:call ((foo foo) env)
  `(200 (("Lisp" . "Common Lisp"))
        ("<h1>" "Hello " ,(get-universal-time) "</h1>")))

(lack:run (make-instance 'lack-hunchentoot:hunchentoot-web-server)
          (make-instance 'foo))

