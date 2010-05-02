(in-package :blog)

(defclass top (action-controller)
  ((message)))

(defmethod index ((self top))
  (with-slots (message) self
    (setf message "Hello")))
