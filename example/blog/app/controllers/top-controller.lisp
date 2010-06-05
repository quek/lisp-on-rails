(in-package :blog)

(defclass top-controller (application-controller)
  ((message)
   (posts)))

(defmethod index ((self top-controller))
  (with-slots (message posts) self
    (setf message "まみむめも♪"
          posts (all post))))


#|
(def-controller top (application)
  (def-action index
      (setf @message "まみむめも♪"
            @posts (all post)))
  (def-action foo
      (setf @essage "foo")))
|#
