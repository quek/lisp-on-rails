(in-package :action-controller)

(defgeneric param (key)
  (:method ((key symbol))
    (param (string-downcase (symbol-name key))))
  (:method ((key string))
    (lack:param *request* key)))

(defgeneric (setf param) (value key)
  (:method (value (key symbol))
    (setf (param (string-downcase (symbol-name key))) value))
  (:method (value (key string))
    (setf (param key) value)))

(defclass dispatcher ()
  ())

(defmethod lack:call ((dispatcher dispatcher) env)
  (let ((*request* (lack:make-request env))
        (*response* (lack:make-response)))
    (aif (loop for i in *routes*
            with url = (lack:url *request*)
            thereis (funcall i url))
         (apply #'dispatch dispatcher it)
         (error "no route for ~a" (lack:url *request*)))))

(defmethod dispatch ((dispatcher dispatcher) &key controller (action "INDEX"))
  (let ((class (intern #"""#,controller,-CONTROLLER""" *app-package*))
        (method (intern action *app-package*)))
    (clsql-sys:with-database (clsql-sys:*default-database*
                              active-record::*connection-spec*
                              :make-default t
                              :pool t
                              :encoding :utf-8)
      ;; TODO ↓ どっかちゃんとした場所で  ↑ もどうかな？
      (mapc #'clsql-sys:execute-command
          '("set character_set_client='utf8'"
            "set character_set_connection='utf8'"
            "set character_set_results='utf8'"))
      (perform-action (make-instance class) method))
    (lack:finish *response*)))
