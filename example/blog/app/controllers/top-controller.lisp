(in-package :blog)

(defclass top-controller (application-controller)
  ((message)
   (posts)))

(defmethod index ((self top-controller))
  (with-slots (message posts) self
    (setf message "Hello")
    (setf posts (all post))))
