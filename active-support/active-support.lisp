(in-package :active-support)

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

(defmacro with-keywords-removed ((options &rest keywords) &body body)
  `(let ((,options (loop for (k v) on ,options by #'cddr
                         unless (member k ',keywords :test #'eq)
                           append (list k v))))
     ,@body))
