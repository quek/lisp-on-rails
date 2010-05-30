(in-package :lack-hunchentoot)

(setf hunchentoot:*hunchentoot-default-external-format*
      (flexi-streams:make-external-format :utf-8)
      hunchentoot:*default-content-type* "text/html; charset=utf-8"
      hunchentoot:*show-lisp-errors-p* t)

(defvar *env*)

(defclass hunchentoot-web-server (web-server) ())

(defmethod run ((web-server hunchentoot-web-server) x)
  (setf lack::*web-server* web-server)
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
  (setf (hunchentoot:return-code*) status)
  (loop for (k . v) in header
     do (setf (hunchentoot:header-out k) v))
  (with-output-to-string (out)
    (loop for i in body
       if i
         do (princ i out))))

(defclass hunchentoot-request (request)
  ((params :initarg :params :accessor params)
   (get-params :initarg :get-params :accessor get-params)
   (post-params :initarg :post-params :accessor post-params)
   (cookie :initarg :cookie :accessor cookie)
   (query-string :initarg :query-string :accessor query-string)))

(defmethod lack::make-request-for-web-server (env (web-server
                                                   hunchentoot-web-server))
  (let ((get-params (alexandria:alist-hash-table
                     (hunchentoot:get-parameters*) :test #'equal))
        (post-params (alexandria:alist-hash-table
                      (hunchentoot:post-parameters*) :test #'equal))
        (params (alexandria:alist-hash-table
                 (append (hunchentoot:get-parameters*)
                         (hunchentoot:post-parameters*)) :test #'equal))
        (cookie (alexandria:alist-hash-table
                 (hunchentoot:cookies-in*) :test #'equal))
        (query-string (hunchentoot:query-string*)))
    (make-instance 'hunchentoot-request
                   :params params
                   :get-params get-params
                   :post-params post-params
                   :cookie cookie
                   :query-string query-string)))

(defmethod request-method (request)
  (intern (string-upcase (string (hunchentoot:request-method*))) :keyword))

(defmethod url (request)
  (hunchentoot:request-uri*))

#|
(defclass foo () ())

(defmethod call ((foo foo) env)
  `(200 (("Lisp" . "Common Lisp"))
        ("<h1>" "Hello " ,(get-universal-time) "</h1>")))

(lack:run (make-instance 'lack-hunchentoot:hunchentoot-web-server)
          (make-instance 'lack-hunchentoot::foo))
|#
