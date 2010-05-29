(in-package :action-view)

(defun <%?-readtable ()
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\= '<%=-reader)
    (set-macro-character #\# '<%#-reader)
    (set-macro-character #\% '%>-reader)
    *readtable*))

(defun <%=-reader (stream char)
  (declare (ignore char))
  (setf *readtable* (copy-readtable nil))
  `(out
    (progn
      ,@(loop for c = (peek-char t stream t t t)
              if (char/= c #\%)
                collect (read stream t t t)
              else if (char= #\> (progn
                                   (read-char stream) ; %
                                   (peek-char nil stream t t t)))
                     do (read-char stream) ; >
                        (setf *readtable* (make-html-readtable stream))
                        (loop-finish)
              else
                collect (progn
                          (unread-char #\% stream)
                          (read stream t t t))))))

(defun <%#-reader (stream char)
  (declare (ignore char))
  (loop for % = (read-char stream) then c
        for c = (read-char stream)
        until (and (char= #\% %)
                   (char= #\> c)))
  (setf *readtable* (make-html-readtable stream))
  (read stream t t t))

(defun %>-reader (stream char)
  (declare (ignore char))
  (if (char= #\> (peek-char nil stream t t t))
      (progn
        (read-char stream)              ; >
        (setf *readtable* (make-html-readtable stream))
        (read stream nil stream t))
      (progn
        (unread-char #\% stream)
        (setf *readtable* (copy-readtable nil))
        (read stream t t t))))

(defparameter *<%?-readtable* (<%?-readtable))


(defun char-reader (stream char)
  (unread-char char stream)
  `(out ,(with-output-to-string (out)
           (loop for c = (read-char stream nil nil t)
                 while c
                 if (and (char= #\< c)
                         (char= #\% (peek-char nil stream nil nil t)))
                   do (setf *readtable*  *<%?-readtable*)
                      (read-char stream) ; %
                      (loop-finish)
                 else
                   do (write-char c out)))))

(defgeneric make-html-readtable (x)
  (:method ((char character))
    (let ((*readtable* (copy-readtable nil)))
      (set-macro-character char 'char-reader)
      *readtable*))
  (:method ((stream stream))
    (make-html-readtable (peek-char nil stream nil nil t)))
  (:method ((x null))
    (copy-readtable nil)))

(defun out (x)
  (princ x))

(defun out-if (x)
  (when x (out x)))

(defun html-defun-readtable (fname pathspec)
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character
     (first-char pathspec)
     (lambda (stream char)
       (unread-char char stream)
       (print
        `(defun ,fname ()
           ,@(let ((*readtable* (make-html-readtable char)))
               (loop for x = (read stream nil stream t)
                     until (eq x stream)
                     collect x))))))
    *readtable*))

(defun first-char (pathspec)
  (with-open-file (in pathspec)
    (read-char in)))

(defun load-html (fname pathspec &rest args)
  (let ((*readtable* (html-defun-readtable fname pathspec)))
    (apply #'load pathspec args)
    fname))

(defun compile-html-file (fname input-file &rest args)
  (let ((*readtable* (html-defun-readtable fname input-file)))
    (apply #'compile-file input-file args)))
