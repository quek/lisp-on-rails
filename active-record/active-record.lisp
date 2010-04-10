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

(defmethod c2mop:validate-superclass ((class standard-class)
                                      (super active-record-class))
  nil)




(defclass ar-slot-mixin ()
  ((column :accessor slot-definition-column
           :initarg :column
           :initform nil)))

(defclass ar-direct-slot-definition
    (ar-slot-mixin c2mop:standard-direct-slot-definition)
  ())

(defclass ar-effective-slot-definition
    (ar-slot-mixin c2mop:standard-effective-slot-definition)
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

(defmacro def-has-xxx-slot-definition (xxx
                                       default-class-symbol-form)
  `(progn
     (defclass ,(sym "ar-has-" xxx "-slot-mixin") ()
       ((,(sym "has-" xxx) :initarg ,(key-sym "has-" xxx)
          :initform nil
          :accessor ,(sym "has-" xxx))
        (class-symbol :initarg :class-symbol
                      :initform nil
                      :accessor class-symbol)))

     (defmethod initialize-instance :after ((self ,(sym "ar-has-" xxx
                                                        "-slot-mixin"))
                                            &rest args)
       (declare (ignore args))
       (unless (class-symbol self)
         (setf (class-symbol self) ,default-class-symbol-form)))

     (defclass ,(sym "ar-has-" xxx "-direct-slot-definition")
         (ar-direct-slot-definition ,(sym "ar-has-" xxx "-slot-mixin"))
       ())

     (defclass ,(sym "ar-has-" xxx "-effective-slot-definition")
         (ar-effective-slot-definition ,(sym "ar-has-" xxx "-slot-mixin"))
       ())
     ))

(def-has-xxx-slot-definition one (has-one self))
(def-has-xxx-slot-definition many (sym (singularize (has-many self))))

(defmethod c2mop:direct-slot-definition-class ((class active-record-class)
                                               &rest initargs)
  (find-class
   (cond ((getf initargs :belongs-to)
          'ar-belongs-to-direct-slot-definition)
         ((getf initargs :has-many)
          'ar-has-many-direct-slot-definition)
         ((getf initargs :has-one)
          'ar-has-one-direct-slot-definition)
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
      (ar-has-one-direct-slot-definition
       (let* ((*effective-slot-definition-class*
               'ar-has-one-effective-slot-definition)
              (esd (call-next-method)))
         (setf (has-one esd) (has-one dslotd)
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
                  :conditions (list (key-sym (class-name class) '-id)
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

(defmethod c2mop:slot-value-using-class
    ((class active-record-class)
     instance
     (slot-def ar-has-one-effective-slot-definition))
  (aif (call-next-method)
       it
       (setf (slot-value instance (has-one slot-def))
             (car (all (find-class (class-symbol slot-def))
                       :conditions (list (key-sym (class-name class) '-id)
                                         (%value-of instance :id)))))))

(defmethod (setf c2mop:slot-value-using-class) :after
    (new-value
     (class active-record-class)
     instance
     (slot-def ar-has-one-effective-slot-definition))
  (when new-value
    (setf (%value-of new-value (str (class-name class) "-id"))
          (%value-of instance :id))))


(define-method-combination active-record ()
  ((around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after)))
  "before メソッドが nil を返した場合メソッドの実行を中断する。"
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods))
         (call-methods-and (methods)
           `(and ,@(mapcar #'(lambda (method)
                               `(call-method ,method))
                           methods))))
    (let ((form (if (or before after (rest primary))
                    `(when ,(call-methods-and before)
                       (multiple-value-prog1
                           (call-method ,(first primary)
                                        ,(rest primary))
                         ,@(call-methods (reverse after))))
                    `(call-method ,(first primary)))))
      (if around
          `(call-method ,(first around)
                        (,@(rest around)
                           (make-method ,form)))
          form))))


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

(defgeneric coerce-sql-symbol (x)
  (:method ((x string))
    x)
  (:method ((x null))
    "null")
  (:method ((x symbol))
    (substitute #\_ #\- (string-downcase (symbol-name x))))
  (:method ((x clsql-sys:wall-time))
    (clsql-sys::db-timestring x))
  (:method (x)
    x))

(defgeneric coerce-sql-value (x)
  (:method ((x string))
    (with-output-to-string (out)
      (write-char #\' out)
      (map nil (lambda (x)
                 (if (char= x #\')
                     (write-string "''" out)
                     (write-char x out)))
           x)
      (write-char #\' out)))
  (:method ((x cons))
    (format nil "(~{~a~^, ~})" (mapcar #'coerce-sql-value x)))
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
    (setf (%new-record-p instance) nil)
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


(defun construct-finder-sql (class
                             &key
                             conditions
                             order
                             group
                             having
                             limit
                             offset
                             ;;joins
                             ;;include
                             (select "*")
                             (from (%table-name-of class))
                             ;;readonly
                             lock)
  (with-output-to-string (out)
    (format out "select ~a from ~a" select from)
    (when conditions
      (format out " where ~{~a~^ and ~}"
              (loop for (k v) on conditions by #'cddr collect
                   (str (coerce-sql-symbol k)
                        (typecase v
                          (cons " in ")
                          (t "="))
                        (coerce-sql-value v)))))
    (when group
      (format out " group by ~a" group))
    (when having
      (format out " having ~a" having))
    (when order
      (format out " order by ~a" order))
    (when limit
      (format out " limit ~a" limit))
    (when offset
      (format out " offset ~a" offset))
    (when lock
      (format out " for update"))))

(defgeneric reverse-sql-order (order)
  (:method
      ((order symbol))
    (reverse-sql-order (coerce-sql-symbol order)))
  (:method
      ((order string))
    (format nil "~{~a~^, ~}"
            (loop for i in (ppcre:split "," order) collect
                 (multiple-value-bind (s m)
                     (ppcre:regex-replace
                      (ppcre:create-scanner "\\sasc$" :case-insensitive-mode t)
                      i "desc")
                   (if m s
                       (multiple-value-bind (s m)
                           (ppcre:regex-replace
                            (ppcre:create-scanner "\\s(edsc|DESC)$"
                                                  :case-insensitive-mode t)
                            i "asc")
                         (if m s
                             (str i " desc")))))))))

;; find は CL にあるので select にする
(defgeneric select (class id-or-keyword
                          &key
                          conditions
                          order
                          group
                          having
                          limit
                          offset
                          joins
                          include
                          select
                          from
                          readonly
                          lock
                          &allow-other-keys))
(defmethod select ((class symbol) id-or-keyword &rest args)
  (apply #'select (find-class class) id-or-keyword args))
(defmethod select ((class active-record-class) (id integer)
                   &rest args
                   &key conditions)
  (setf conditions (append (list :id id) conditions))
  (multiple-value-bind (rows columns)
      (clsql-sys:query
       ;; TODO きっと args から conditions を取り除かなければならない。
       ;; McCLIM で綺麗にやってた気がする。
       (with-keywords-removed (args :conditions)
         (apply #'construct-finder-sql class :conditions conditions args)))
    ;; TODO 該当なしなら例外
    (when rows
      (make-instance-from-row class (car rows) columns))))

(defmethod select ((class active-record-class) (ids list)
                   &rest args
                   &key conditions)
  (setf conditions (append (list :id ids) conditions))
  (multiple-value-bind (rows columns)
      (clsql-sys:query
       (with-keywords-removed (args :conditions)
         (apply #'construct-finder-sql class :conditions conditions args)))
    ;; TODO 該当なしなら例外
    (loop for i in rows
       collect (make-instance-from-row class i columns))))

(defmethod select ((class active-record-class) (keyword (eql :all))
                   &rest args)
  (multiple-value-bind (rows columns)
      (clsql-sys:query
       (apply #'construct-finder-sql class args))
    (loop for i in rows
       collect (make-instance-from-row class i columns))))

(defmethod select ((class active-record-class) (keyword (eql :first))
                   &rest args
                   &key (order "id"))
  (multiple-value-bind (rows columns)
      (clsql-sys:query
       (with-keywords-removed (args :order :limit)
         (apply #'construct-finder-sql class :order order :limit 1 args)))
    (when rows
      (make-instance-from-row class (car rows) columns))))

(defmethod select ((class active-record-class) (keyword (eql :last))
                   &rest args
                   &key (order "id"))
  (multiple-value-bind (rows columns)
      (clsql-sys:query
       (with-keywords-removed (args :order :limit)
         (apply #'construct-finder-sql class :order (reverse-sql-order order)
                :limit 1 args)))
    (when rows
      (make-instance-from-row class (car rows) columns))))

(defun all (&rest args)
  (apply #'select (car args) :all (cdr args)))
;; first と last は CL パッケージとかぶる。

(defgeneric save (record)
  (:method-combination active-record)
  (:method ((self base))
    (create-or-update self)))

(defgeneric create-or-update (record)
  (:method-combination active-record)
  (:method ((self base))
    (if (%new-record-p self)
        (create self)
        (update self))))

(defmethod save :before (record)
  (let* ((class (class-of record))
         (created-at (find-column class :created-at))
         (updated-at (find-column class :updated-at))
         (time (clsql-sys::get-time)))
    (when (and created-at (null (%value-of record created-at)))
      (setf (%value-of record created-at) time))
    (when updated-at
      (setf (%value-of record updated-at) time))
    t))

(defgeneric create (record)
  (:method-combination active-record)
  (:method ((self base))
    (let ((slots (mapcar #'column-name (columns-expect-id (class-of self)))))
      (clsql-sys:execute-command
       (format nil "insert into ~a (~{~a~^,~}) values (~{~a~^,~})"
               (%table-name-of self)
               (mapcar #'coerce-sql-symbol slots)
               (mapcar (lambda (x) (coerce-sql-value (slot-value self x)))
                       slots)))
      (setf (%new-record-p self) t
            (%value-of self :id)
            (caar (clsql-sys:query "select last_insert_id()"))))
    self))

(defgeneric update (record)
  (:method-combination active-record)
  (:method ((self base))
    (let ((slots (mapcar #'column-name (columns-expect-id (class-of self)))))
      (clsql-sys:execute-command
       (format nil "update ~a set ~{~a = ~a~^,~} where id = ~a"
               (%table-name-of self)
               (loop for x in slots
                  append (list (coerce-sql-symbol x)
                               (coerce-sql-value (slot-value self x))))
               (%value-of self :id))))
    self))

(defgeneric destroy (record)
  (:method-combination active-record)
  (:method ((self base))
    (clsql-sys:execute-command (format nil "delete from ~a where id = ~d"
                                       (%table-name-of (class-of self))
                                       (%value-of self :id)))))


(defmacro def-record (name &rest options)
  (let* ((table-name (substitute #\_ #\- (pluralize name)))
         (attributes (clsql-sys:list-attribute-types table-name))
         (columns (loop for (column-name type precision scale nullable)
                     in attributes
                     collect (make-instance
                              'column
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
                               :accessor (sym table '-of))
                 ;; has-one
                 if (eq :has-one association)
                 collect (list table
                               :initform nil
                               :has-one table
                               :class-symbol table
                               :accessor (sym table '-of))
                   ))
           (:metaclass active-record-class)))

       (setf (%table-name-of ,name) ,table-name)
       (setf (%columns-of ,name)
             (loop for (column-name type precision scale nullable)
                in ',attributes
                collect (make-instance
                         'column
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