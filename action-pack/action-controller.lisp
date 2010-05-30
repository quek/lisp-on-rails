(in-package :action-controller)

(defclass base ()
  ((performed-render-p :initform nil :accessor performed-render-p)
   (performed-redirect-p :initform nil :accessor performed-redirect-p)
   (action :accessor action-of)))

(defgeneric perform-action (controller action)
  (:method ((controller base) action)
    (funcall action controller)
    (unless (performedp controller)
      (default-render controller))))

(defmethod perform-action :before ((controller base) action)
  (setf (action-of controller) action))

(defgeneric performedp (controller)
  (:method ((controller base))
    (or (performed-render-p controller)
        (performed-redirect-p controller))))

(defgeneric default-render (controller)
  (:method ((controller base))
    (render controller)))

(defgeneric render (controller &key :action :html :layout &allow-other-keys)
  (:method (controller &rest args)
    (declare (ignore args))
    (let ((fname (view-fname controller)))
      (action-view::load-html fname (view-path controller))
      (funcall fname))))

(defgeneric controller-name-of (controller)
  (:method ((controller base))
    (multiple-value-bind (_ name)
        (ppcre:scan-to-strings "(.*)-controller$"
                               (string-downcase
                                (class-name (class-of controller))))
      (declare (ignore _))
      (elt name 0))))

(defgeneric action-name-of (controller)
  (:method ((controller base))
    (string-downcase
     (string (action-of controller)))))

(defgeneric view-path (controller)
  (:method ((controller base))
    (let ((controller-name (controller-name-of controller))
          (action-name (action-name-of controller)))
      (merge-pathnames
       #"""app/views/#,controller-name,/#,action-name,.html"""
       cl-user::*rails-root*))))


(defgeneric view-fname (controller)
  (:method ((controller base))
    (let ((controller-name (controller-name-of controller))
          (action-name (action-name-of controller)))
      (intern #"""views/#,controller-name,/#,action-name"""))))
