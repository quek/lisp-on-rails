(in-package :action-pack)

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
  ())

#|
dispatcher.rb
call build_middleware_stack _call dispatch Routing::Routes.call(@env)
|#

(defmethod call ((dispatcher dispatcher) env)
  (let ((*request* (lack:make-request env))
        (*response* (lack:make-response)))
    (dispatch)))

(defun compute-route ()
  (values 'controller 'action))         ; TODO

(defun dispatch ()
  (multiple-value-bind (controller action) (compute-route)
    (setf (param :controller) controller
          (param :action) action)
    (perform-action (make-instance controller) action)))
