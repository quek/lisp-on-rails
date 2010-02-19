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
  (:method (x)
    (pluralize (string-downcase (string x)))))

(defmethod pluralize ((string string))
  (let* (flush add
         (len (length string))
	 (last-char-raw (char string (1- len)))
	 (last-char (char-upcase last-char-raw))
	 (penult-char (char-upcase (if (> len 1)
				       (char string (- len 2))
				       #\Nul))) ;dummy
	 (last-3 (subseq string (max 0 (- len 3)))))
    (declare (character last-char-raw last-char penult-char)
             (string last-3))
    (setf (values flush add)
          (cond ((and (char-equal last-char #\Y)
                      (not (member penult-char '(#\A #\E #\I #\O #\U))))
                 (values 1 "ies"))
                ((or (string-equal string "ox")
                     (string-equal string "vax"))
                 (values nil "en"))
                ((or (and (char= last-char #\H)
                          (member penult-char '(#\C #\S)))
                     (member last-char '(#\S #\Z #\X)))
                 (values nil "es"))
                ((string-equal last-3 "man")
                 (values 2 "en"))
                ((string-equal last-3 "ife")
                 (values  2 "ves"))
                (t (values nil "s"))))
    (when flush
      (setq string (subseq string 0 (- len flush))))
    (concatenate 'string string add)))

(defgeneric singularize (x)
  (:method (x)
    (singularize (string-downcase (string x))))
  (:method ((string string))
    (subseq string 0 (1- (length string)))))

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
          do (write-string (string-downcase (princ-to-string i)) out))))

(defun sql->sym (name &key (package *package* packagep) (downcase nil))
  (flet ((normalize-case (str)
           (funcall (if downcase #'string-downcase #'string-upcase) 
                    str))
         (subst-hyphen (x)
           (substitute #\- #\_ x)))
    (intern (subst-hyphen (normalize-case (string name)))
            (if packagep package *package*))))

(defclass column ()
  ((name :initarg :name :accessor column-name)
   (name-string :initarg :name-string :accessor column-name-string)
   (key :initarg :key :accessor column-key)
   (type :initarg :type :accessor column-type)
   (precision :initarg :precision :initform nil :accessor column-precision)
   (scale :initarg :scale :initform nil :accessor column-scale)
   (nullable :initarg :nullable :initform nil :accessor column-nullable-p)))

(defclass active-record-class (standard-class)
  ((%table-name :initarg :%table-name :accessor %table-name-of)
   (%columns :initarg :%columns :accessor %columns-of)))

(defmethod print-object ((class active-record-class) stream)
  (print-unreadable-object (class stream :type t)
    (format stream "~a ~{~a~^ ~}" (class-name class)
            (mapcar #'column-name (%columns-of class)))))

(defmethod c2mop:validate-superclass ((class active-record-class) (super standard-class))
  t)

(defmethod c2mop:validate-superclass ((class standard-class) (super active-record-class))
  nil)






(defclass ar-slot-mixin ()
  ((column :accessor slot-definition-column
           :initarg :column
           :initform nil)))

(defclass ar-direct-slot-definition
      (ar-slot-mixin
       c2mop:standard-direct-slot-definition)
  ())

(defclass ar-effective-slot-definition
      (ar-slot-mixin
       c2mop:standard-effective-slot-definition)
  ())

(defclass ar-belongs-to-slot-mixin ()
  ((belongs-to :initarg :belongs-to
               :initform nil
               :accessor belongs-to)
   (foreign-slotname :initarg :foreign-slotname
                     :initform nil
                     :accessor foreign-slotname)))

(defclass ar-belongs-to-direct-slot-definition (ar-direct-slot-definition
                                                ar-belongs-to-slot-mixin)
  ())

(defclass ar-belongs-to-effective-slot-definition (ar-effective-slot-definition
                                                   ar-belongs-to-slot-mixin)
  ())

(defclass ar-has-many-slot-mixin ()
  ((has-many :initarg :has-many
             :initform nil
             :accessor has-many)
   (class-symbol :initarg :class-symbol
                 :initform nil
                 :accessor class-symbol)))

(defmethod initialize-instance :after ((self ar-has-many-slot-mixin) &rest args)
  (declare (ignore args))
  (unless (class-symbol self)
    (setf (class-symbol self)
          (sym (singularize (has-many self))))))

(defclass ar-has-many-direct-slot-definition (ar-direct-slot-definition
                                              ar-has-many-slot-mixin)
  ())

(defclass ar-has-many-effective-slot-definition (ar-effective-slot-definition
                                                 ar-has-many-slot-mixin)
  ())

(defmethod c2mop:direct-slot-definition-class ((class active-record-class)
                                               &rest initargs)
  (find-class
   (cond ((getf initargs :belongs-to)
          'ar-belongs-to-direct-slot-definition)
         ((getf initargs :has-many)
          'ar-has-many-direct-slot-definition)
         (t 'ar-direct-slot-definition))))

;; compute-effective-slot-definition-initargs がポータブルではないので
(defvar *effective-slot-definition-class* nil)

(defmethod c2mop:effective-slot-definition-class ((class active-record-class)
                                                  &rest initargs)
  (declare (ignore initargs))
  (find-class
   (or *effective-slot-definition-class*
       'ar-effective-slot-definition)))

(defmethod c2mop:compute-effective-slot-definition
  ((class active-record-class)
   slot-name
   direct-slot-definitions)
  (let ((dslotd (car direct-slot-definitions)))
    (typecase dslotd
      (ar-belongs-to-direct-slot-definition
         (let* ((*effective-slot-definition-class*
                 'ar-belongs-to-effective-slot-definition)
                (esd (call-next-method)))
           (setf (belongs-to esd) (belongs-to dslotd))
           (setf (foreign-slotname esd) (foreign-slotname dslotd))
           esd))
      (ar-has-many-direct-slot-definition
         (let* ((*effective-slot-definition-class*
                 'ar-has-many-effective-slot-definition)
                (esd (call-next-method)))
           (setf (has-many esd) (has-many dslotd)
                 (class-symbol esd) (class-symbol dslotd))
           esd))
      (t (setf *effective-slot-definition-class* nil)
         (call-next-method)))))

(defmethod c2mop:slot-value-using-class
  ((class active-record-class)
   instance
   (slot-def ar-belongs-to-effective-slot-definition))
  (aif (call-next-method)
       it
       (setf (slot-value instance (belongs-to slot-def))
             (select (find-class (belongs-to slot-def))
                     (slot-value instance (foreign-slotname slot-def))))))

(defmethod (setf c2mop:slot-value-using-class) :after
           (new-value
            (class active-record-class)
            instance
            (slot-def ar-belongs-to-effective-slot-definition))
  (setf (slot-value instance (foreign-slotname slot-def))
        (and new-value (%value-of new-value :id))))

(defmethod c2mop:slot-value-using-class
  ((class active-record-class)
   instance
   (slot-def ar-has-many-effective-slot-definition))
  (aif (call-next-method)
       it
       (setf (slot-value instance (has-many slot-def))
             (all (find-class (class-symbol slot-def))
                  :conditons (list (key-sym (class-name class) '-id)
                                   (%value-of instance :id))))))

(defmethod (setf c2mop:slot-value-using-class) :after
           (new-value
            (class active-record-class)
            instance
            (slot-def ar-has-many-effective-slot-definition))
  (loop with id = (%value-of instance :id)
        with column = (str (class-name class) "-id")
        for x in new-value
        do (setf (%value-of x column) id)))




(defparameter base
  (defclass base ()
    ((%new-record :initarg :%new-record  :initform t :accessor %new-record-p))
    (:metaclass active-record-class)))

(defmethod %table-name-of ((x base))
  (%table-name-of (class-of x)))

(defmethod %columns-of ((x base))
  (%columns-of (class-of x)))

(defgeneric %value-of (record column)
  (:method (record (column symbol))
    (%value-of record (symbol-name column)))
  (:method (record (column string))
    (%value-of record (find-column (class-of record) column)))
  (:method (record (column column))
    (slot-value record (column-name column))))

(defgeneric (setf %value-of) (value record column)
  (:method (value record (column symbol))
    (setf (%value-of record (symbol-name column)) value))
  (:method (value record (column string))
    (setf (%value-of record (find-column (class-of record) column)) value))
  (:method (value record (column column))
    (setf (slot-value record (column-name column)) value)))

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
  (:method ((x clsql-sys:wall-time))
    (clsql-sys::db-timestring x))
  (:method (x)
    x))

(defmethod make-instance-from-row (class row fields)
  (let ((instance (make-instance class)))
    (loop for col in fields
          for i from 0
          for x = (find-column class col)
          if x
            do (setf (%value-of instance x) (nth i row)))
    instance))

(defgeneric find-column (active-record-class x)
  (:method ((class active-record-class) (x symbol))
    (find-column class (symbol-name x)))
  (:method ((class active-record-class) (x string))
    (find (substitute #\_ #\- (string-downcase x))
          (%columns-of class) :key #'column-name-string
          :test #'string=)))

(defgeneric columns-expect-id (active-record-class)
  (:method ((class active-record-class))
    (loop for x in (%columns-of class)
          unless (string= "id" (column-name-string x))
            collect x)))

;; find は CL にあるので select にする
(defmethod select ((class active-record-class) id)
  (multiple-value-bind (rows columns)
      (clsql-sys:query (format nil "select * from ~a where id = ~d"
                               (%table-name-of class) id))
    (when rows
      (make-instance-from-row class (car rows) columns))))

(defgeneric all (class &key &allow-other-keys)
  (:method ((class active-record-class) &key conditions)
    (multiple-value-bind (rows columns)
        (clsql-sys:query
         (with-output-to-string (out)
           (format out "select * from ~a" (%table-name-of class))
           (when conditions
             (format out " where ~{~a = ~a~^ and ~}" conditions))))
      (loop for i in rows
            collect (make-instance-from-row class i columns)))))

(defgeneric save (record)
  (:method ((self base))
    (let ((slots (mapcar #'column-name (columns-expect-id (class-of self)))))
      (if (%new-record-p self)
          (progn
            (clsql-sys:execute-command
             (format nil "insert into ~a (~{~a~^,~}) values (~{~a~^,~})"
                     (%table-name-of self)
                     (mapcar #'coerce-sql slots)
                     (mapcar (lambda (x) (coerce-sql (slot-value self x)))
                             slots)))
            (setf (%new-record-p self) t
                  (%value-of self :id)
                  (caar (clsql-sys:query "select last_insert_id()"))))
          ;; TODO update
          ))
    self))

(defmethod save :before (record)
  (let* ((class (class-of record))
         (created-at (find-column class :created-at))
         (updated-at (find-column class :updated-at))
         (time (clsql-sys::get-time)))
    (when (and created-at (null (%value-of record created-at)))
      (setf (%value-of record created-at) time))
    (when updated-at
      (setf (%value-of record updated-at) time))))

(defgeneric destroy (record)
  (:method ((self base))
    (clsql-sys:execute-command (format nil "delete from ~a where id = ~d"
                                       (%table-name-of (class-of self))
                                       (%value-of self :id)))))

(defmacro def-record (name &rest options)
  (let* ((table-name (pluralize name))
         (attributes (clsql-sys:list-attribute-types table-name))
         (columns (loop for (column-name type precision scale nullable) in attributes
                        collect (make-instance 'column
                                     :name (sql->sym column-name :package *package*)
                                     :name-string column-name
                                     :key (sql->sym column-name :package :keyword)
                                     :type type
                                     :precision precision
                                     :scale scale
                                     :nullable nullable))))
    `(progn
       (defparameter ,name
         (defclass ,name (base)
           (,@(loop for x in columns
                    collect (list (column-name x)
                                  :initarg (column-key x)
                                  :initform nil
                                  :accessor (sym (column-name x) '-of)))
            ,@(loop for (association table) in options
                    ;; belongs-to
                    if (eq :belongs-to association)
                      collect (list table
                                    :initform nil
                                    :belongs-to table
                                    :foreign-slotname (sym table '-id)
                                    :accessor (sym table '-of))
                    ;; has-many
                    if (eq :has-many association)
                      collect (list table
                                    :initform nil
                                    :has-many table
                                    :class-symbol (sym (singularize table))
                                    :accessor (sym table '-of)))
            )
           (:metaclass active-record-class)))

       (setf (%table-name-of ,name) ,table-name)
       (setf (%columns-of ,name)
             (loop for (column-name type precision scale nullable) in ',attributes
                   collect (make-instance 'column
                                :name (sql->sym column-name :package *package*)
                                :name-string column-name
                                :key (sql->sym column-name :package :keyword)
                                :type type
                                :precision precision
                                :scale scale
                                :nullable nullable)))

       (defmethod print-object ((self ,name) stream)
         (print-unreadable-object (self stream :type t :identity t)
           (format stream "~@{~a: ~s~^, ~}"
                   ,@(loop for x in columns
                           append (list `',(column-name x)
                                        `(,(sym (column-name x)'-of) self)))))))))

;; (def-record post (:has-many comments))
;; (def-record comment (:belongs-to post))
;; (select post 1)
;; (save (make-instance 'post :name "名前" :title "タイトル" :content "内容"))
;; (post-all)