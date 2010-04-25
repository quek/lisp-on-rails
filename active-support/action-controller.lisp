(in-package :active-support)

(defclass action-controller ()
  ((performed-render-p :initform nil :accessor performed-render-p)
   (performed-redirect-p :initform nil :accessor performed-redirect-p)))

(defgeneric perform-action (controller action)
  (:method ((controller action-controller) action)
    (funcall action controller)
    (unless (performedp controller)
      (default-render controller))))

(defgeneric performedp (controller)
  (:method ((controller action-controller))
    (or (performed-render-p controller)
        (performed-redirect-p controller))))

(defgeneric default-render (controller)
  (:method ((controller action-controller))
    (render controller)))

(defgeneric render (controller &key :action :html :layout))
