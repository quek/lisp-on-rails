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
  ((%table-name :initarg :%table-name :allocation :class :accessor %table-name-of)
   (%attributes :initarg :%attributes :allocation :class :accessor %attributes-of)
   (%slots :initarg :%slots :allocation :class :accessor %slots-of)
   (%accessors :initarg :%accessors :allocation :class :accessor %accessors-of)
   (%new-record :initarg :%new-record  :initform t :accessor %new-record-p)))

(defgeneric coerce-sql (x)
  (:method ((x string))
    (with-output-to-string (out)
      (write-char #\' out)
      (map nil (lambda (x)
                 (if (char= x #\')
                     (write-string "''" out)
                     (write-char x out)))
           x)
      (write-char #\' out)))
  (:method ((x null))
    "null")
  (:method ((x symbol))
    (substitute #\_ #\- (string-downcase (symbol-name x))))
  (:method (x)
    x))

(defgeneric save (record)
  (:method ((self base))
    (let ((slots (remove-if (lambda (x) (equalp "ID" (symbol-name x))) (%slots-of self))))
      (if (%new-record-p self)
          (clsql-sys:execute-command
           (format nil "insert into ~a (~{~a~^,~}) values (~{~a~^,~})"
                   (%table-name-of self)
                   (mapcar #'coerce-sql slots)
                   (mapcar (lambda (x) (coerce-sql (slot-value self x)))
                           slots)))
          ;;update
          ))))

(defmacro def-record (name)
  (let* ((table-name (pluralize name))
         (attributes (clsql-sys:list-attribute-types table-name)))
    (loop for (x) in attributes do (nsubstitute #\- #\_ x))
    `(progn
       (defclass ,name (base)
         (,@(loop for (column-name type precision scale nullable) in attributes
                  collect (list (sym column-name)
                                :initarg (key-sym column-name)
                                :initform nil
                                :accessor (sym column-name '-of))))
         (:default-initargs
             :%table-name ,table-name
           :%attributes ',attributes
           :%slots ',(mapcar #'(lambda (x) (sym (car x))) attributes)
           :%accessors ',(mapcar #'(lambda (x) (sym (car x) '-of)) attributes)))

       (defmethod print-object ((self ,name) stream)
         (print-unreadable-object (self stream :type t :identity t)
           (format stream "~@{~a: ~s~^, ~}"
                   ,@(loop for (column-name) in attributes
                           append (list column-name `(,(sym column-name '-of) self))))))

       (defun ,(sym name '-all) ()
         (loop for rs in (clsql-sys:query ,(format nil "select * from ~a" table-name))
               collect (make-instance ',name
                            ,@(loop for (column-name) in attributes
                                    for i from 0
                                    append (list (key-sym column-name) `(nth ,i rs)))))))))
;; (def-record post)
;; (save (make-instance 'post :name "名前" :title "タイトル" :content "内容"))
;; (post-all)