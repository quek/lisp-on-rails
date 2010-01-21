(in-package :active-record)

(defvar *connection-spec* '("localhost" "blog_development" "root" ""))
(setq clsql-sys:*default-database-type* :mysql)

(defgeneric after-connect (database-type)
  (:method ((database-type (eql :mysql)))
    (mapc #'clsql-sys:execute-command
          '("set character_set_client='utf8'"
            "set character_set_connection='utf8'"
            "set character_set_results='utf8'"))))

(defun establish-connection (&optional (connection-spec *connection-spec*)
                             (database-type clsql-sys:*default-database-type*))
  (clsql-sys:connect connection-spec
                     :database-type database-type
                     :if-exists :old)
  (after-connect database-type))

(defgeneric pluralize (x)
  (:method ((x string))
    (concatenate 'string x "s"))
  (:method (x)
    (pluralize (string-downcase (string x)))))

(defun sym (&rest args)
  (intern
   (with-output-to-string (out)
    (loop for x in args
          do (write-string (string-upcase (string x)) out)))
   *package*))

(defun key-sym (&rest args)
  (let ((*package* (find-package :keyword)))
    (apply #'sym args)))

(defun str (&rest args)
  (with-output-to-string (out)
    (loop for i in args
          do (write-string (string-downcase (symbol-name i)) out))))

;;(clsql-sys:list-attribute-types "posts")

(defclass base ()
  ((table-name :initarg :table-name :accessor table-name-of)))

(defmacro def-record (name)
  (let* ((table-name (pluralize name))
         (attributes (clsql-sys:list-attribute-types table-name)))
    `(progn
       (defclass ,(sym table-name) (base)
         (,@(loop for (column-name type precision scale nullable) in attributes
                  collect (list (sym column-name)
                                :initarg (key-sym column-name)
                                :initform nil
                                :accessor (sym column-name '-OF))))
         (:default-initargs
             :table-name ,table-name))
       (defun ,(sym table-name '-all) ()
         (loop for rs in (clsql-sys:query ,(format nil "select * from ~a" table-name))
               collect (make-instance ',(sym table-name)
                            ,@(loop for (column-name) in attributes
                                    for i from 0
                                    append (list (key-sym column-name) `(nth ,i rs)))))))))
;; (def-record post)
;; (posts-all)