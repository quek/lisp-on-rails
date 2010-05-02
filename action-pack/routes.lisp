(in-package :action-pack)

;;(resources :users)
(defun users-path ()
  "/users")
(defun user-path (id)
  #"""/users/#,id""")
'(defun edit-user-path (id)
  #"""/users/#,id,/edit""")
(defun new-user-path (id)
  #"""/users/#,id,/new""")

#|
  map.connect ':controller/:action/:id'
  map.connect ':controller/:action/:id.:format'
  map.connect ':controller/:action.:format'
|#

(let* ((r1 "[-_a-zA-Z0-9]+")
       (r2 (ppcre:create-scanner #""":#,r1""")))
  (defun connect (path &key controller action)
    (let ((parameters (mapcar #'read-from-string
                              (ppcre:all-matches-as-strings r2 path)))
          (reg path))
      (loop for x in parameters
         do (setf reg (ppcre:regex-replace r2 reg #"""(#,r1,)""")))
      (lambda (x)
        (multiple-value-bind (match regs) (ppcre:scan-to-strings reg x)
          (when match
            (append (when controller
                      (list :controller controller))
                    (when action
                      (list :action action))
                    (loop for x in parameters
                       and y across regs
                       append (list x y))))))
      )))

(funcall (connect ":controller/:action/:id") "foo/bar/12")
(funcall (connect "foo/:action/:id" :controller "c1") "foo/bar/12")
