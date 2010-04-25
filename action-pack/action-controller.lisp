(in-package :action-pack)

(defclass action-controller ()
  ())

(defgeneric dispatch (controller request response))

(defmethod call ((controller action-controller) env)
  (let ((*request* (make-request env))
        (*response* (make-response)))
    (dispatch controller *request* *response*)
    (finish *response*)))