(in-package :action-controller)

(defvar *request*)
(defvar *response*)

(defgeneric param (key)
  (:method ((key symbol))
    (param (string-downcase (symbol-name key))))
  (:method ((key string))
    (lack:param *request* key)))

(defgeneric (setf param) (value key)
  (:method (value (key symbol))
    (setf (param (string-downcase (symbol-name key))) value))
  (:method (value (key string))
    (setf (param key) value)))

(defclass dispatcher ()
  ((routes :initarg :routes :accessor routes-of)
   (package :initarg :package :accessor package-of)))

(defmethod call ((dispatcher dispatcher) env)
  (let ((*request* (lack:make-request env))
        (*response* (lack:make-response)))
    (aif (loop for i in *routes*
            with url = (url *request*)
            thereis (funcall i url))
         (apply #'dispatch dispatcher it)
         (error "no route for ~a" (url *request*)))))

(defmethod dispatch ((dispatcher dispatcher) &key controller (action "index"))
  (with-slots (package) dispatcher
    (let ((class (intern controller package))
          (method (intern action package)))
      (perform-action (make-instance class) method)
      (finish *response*))))
