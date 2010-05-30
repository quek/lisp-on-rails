(in-package :blog)

(setf action-controller:*routes-package* (find-package :blog))

(setf action-controller:*routes*
  (list
   (action-controller:connect "/" :controller :top)
   (action-controller:connect ":controller/:action/:id")
   (action-controller:connect ":controller/:action")))
