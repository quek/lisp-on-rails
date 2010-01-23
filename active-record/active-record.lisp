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

(defmethod sb-mop:validate-superclass ((class active-record-class) (super standard-class))
  t)

(defmethod sb-mop:validate-superclass ((class standard-class) (super active-record-class))
  nil)

(defparameter base
  (defclass base ()
    ((%new-record :initarg :%new-record  :initform t :accessor %new-record-p))
    (:metaclass active-record-class)))

(defmethod %table-name-of ((x base))
  (%table-name-of (class-of x)))

(defmethod %columns-of ((x base))
  (%columns-of (class-of x)))

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

(defmethod make-instance-from-row (class row fields)
  (let ((instance (make-instance class))
        (columns (%columns-of class)))
    (loop for col in fields
          for i from 0
          for x = (find col columns :key #'column-name-string :test #'string=)
          if x
            do (setf (slot-value instance (column-name x)) (nth i row)))
    instance))

;; find は CL にあるので select にるす
(defmethod select ((class active-record-class) id)
  (multiple-value-bind (rows columns)
      (clsql-sys:query (format nil "select * from ~a where id = ~d"
                               (%table-name-of class) id))
    (when rows
      (make-instance-from-row class (car rows) columns))))

(defmethod all ((class active-record-class))
  (multiple-value-bind (rows columns)
      (clsql-sys:query (format nil "select * from ~a" (%table-name-of class)))
    (loop for i in rows
          collect (make-instance-from-row class i columns))))

(defgeneric save (record)
  (:method ((self base))
    (let ((slots (mapcar #'column-name  (remove 'id (%columns-of (class-of self))))))
      (if (%new-record-p self)
          (progn
            (clsql-sys:execute-command
             (format nil "insert into ~a (~{~a~^,~}) values (~{~a~^,~})"
                     (%table-name-of self)
                     (mapcar #'coerce-sql slots)
                     (mapcar (lambda (x) (coerce-sql (slot-value self x)))
                             slots)))
            (setf (%new-record-p self) t))
          ;; TODO update
          ))))

(defgeneric destroy (record)
  (:method ((self base))
    (clsql-sys:execute-command (format nil "delete from ~a where id = ~d"
                                       (%table-name-of (class-of self))
                                       (slot-value self 'id)))))

(defmacro def-record (name)
  (let* ((table-name (pluralize name))
         (attributes (clsql-sys:list-attribute-types table-name))
         (columns (loop for (column-name type precision scale nullable) in attributes
                        collect (make-instance 'column
                                     :name (sql->sym column-name :package :active-record)
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
                                  :accessor (sym (column-name x) '-of))))
           (:metaclass active-record-class)))

       (setf (%table-name-of ,name) ,table-name)
       (setf (%columns-of ,name)
             (loop for (column-name type precision scale nullable) in ',attributes
                   collect (make-instance 'column
                                :name (sql->sym column-name :package :active-record)
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

;; (def-record post)
;; (def-record comment)
;; (select post 1)
;; (save (make-instance 'post :name "名前" :title "タイトル" :content "内容"))
;; (post-all)q