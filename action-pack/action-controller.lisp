(in-package :action-controller)

(defclass base ()
  ((performed-render-p :initform nil :accessor performed-render-p)
   (performed-redirect-p :initform nil :accessor performed-redirect-p)))

(defgeneric perform-action (controller action)
  (:method ((controller base) action)
    (funcall action controller)
    (unless (performedp controller)
      (default-render controller))))

(defgeneric performedp (controller)
  (:method ((controller base))
    (or (performed-render-p controller)
        (performed-redirect-p controller))))

(defgeneric default-render (controller)
  (:method ((controller base))
    (render controller)))

(defgeneric render (controller &key :action :html :layout))
