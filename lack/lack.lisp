(in-package :lack)

(defparameter *url-prefix* "/")
(defparameter *port* 8888)

(defclass web-server () ())
(defvar *web-server* (make-instance 'web-server))


(defgeneric call (x env))

(defgeneric run (web-server x))


(defclass request () ())

(defgeneric make-request-for-web-server (env web-server))

(defun make-request (env)
  (make-request-for-web-server env *web-server*))

(defgeneric params (request))
(defgeneric param (request key)
  (:method (request key)
    (gethash key (params request))))
(defgeneric (setf param) (value request key)
  (:method (value request key)
    (setf (gethash key (params request)) value)))
(defgeneric cookies (request))
(defgeneric query-string (request))
(defgeneric body (request))
(defgeneric get-params (request))
(defgeneric post-params (request))

(defgeneric url (request))
(defgeneric scheme (request)
  (:method (request)
    (let ((url (url request)))
      (subseq url 0 (position #\: url)))))
(defgeneric host (request)
  (:method (request)
    (ppcre:scan-to-strings "http?://([^/:]+)" (url request))))
(defgeneric port (request)
  (:method (request)
    (or (parse-integer
         (ppcre:scan-to-strings "http?://[^/:]+:([0-9]+)" (url request))
         :junk-allowed t)
        80)))
(defgeneric fullpath (request)
  (:method (request)
    (or (ppcre:scan-to-strings "http?://[^/]+([^#?]+)" (url request))
        "/")))

(defgeneric request-method (request))
(defgeneric delete-p (request)
  (:method (request)
    (eq :delete (request-method request))))
(defgeneric get-p (request)
  (:method (request)
    (eq :get (request-method request))))
(defgeneric post-p (request)
  (:method (request)
    (eq :post (request-method request))))
(defgeneric put-p (request)
  (:method (request)
    (eq :put (request-method request))))
(defgeneric xhr-p (request)
  (:method (request)
    (eq :xhr (request-method request))))


(defclass response ()
  ((body :initarg :body :initform nil :accessor body)
   (status :initarg :status :initform 200 :accessor status)
   (header :initarg :header
           :initform '(("Content-Type" . "text/html; charset=utf-8;"))
           :accessor header)))

(defgeneric make-response-for-web-server (web-server)
  (:method ((web-server web-server))
    (make-instance 'response)))
(defun make-response ()
  (make-response-for-web-server *web-server*))

(defgeneric out (response string)
  (:method ((response response) string)
    (push string (body response))))

(defgeneric emptyp (response)
  (:method ((response response))
    (null (body response))))

(defgeneric set-cookie (response key value))

(defgeneric delete-cookie (response key))

(defgeneric finish (response)
  (:method ((response response))
    (list (status response)
          (header response)
          (reverse (body response)))))
