(in-package :blog)

(defclass top-controller (application-controller)
  ((message)))

(defmethod index ((self top-controller))
  (with-slots (message) self
    (setf message "Hello")))
