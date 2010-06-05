(in-package :action-view)

(defun basic-readtable ()
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\" 'quek::|#"-reader|)
    *readtable*))

(defun <%?-readtable ()
  (let ((*readtable* (basic-readtable)))
    (set-macro-character #\= '<%=-reader)
    (set-macro-character #\# '<%#-reader)
    (set-macro-character #\% '%>-reader)
    *readtable*))

(defun <%=-reader (stream char)
  (declare (ignore char))
  (setf *readtable* (basic-readtable))
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
        (setf *readtable* (basic-readtable))
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
    (let ((*readtable* (basic-readtable)))
      (set-macro-character char 'char-reader)
      *readtable*))
  (:method ((stream stream))
    (make-html-readtable (peek-char nil stream nil nil t)))
  (:method ((x null))
    (basic-readtable)))

(defun out (x)
  (lack::out action-controller::*response* x))


(defun html-defun-readtable (fname pathspec)
  (let ((*readtable* (basic-readtable)))
    (set-macro-character
     (first-char pathspec)
     (let ((in-package t))
       (lambda (stream char)
         (unread-char char stream)
         (print
          (if in-package
              (progn
                (setf in-package nil)
                `(in-package ,(package-name action-controller:*app-package*)))
              `(defun ,fname ()
                 ,(body-code stream char)))))))
    *readtable*))

(defun body-code (stream char)
  (walk-body-code (read-body-code stream char)))

(defun read-body-code (stream char)
  (let ((*readtable* (make-html-readtable char)))
    (loop for x = (read stream nil stream t)
          until (eq x stream)
          collect x)))

(defun walk-body-code (code)
  `(symbol-macrolet
       ,(series:collect
            (series:mapping
             ((x (series:choose-if (q:^ q:symbol-head-p _ "@")
                                   (series:scan-lists-of-lists-fringe code))))
             `(,x (slot-value action-controller:*controller*
                              ',(intern (subseq (symbol-name x) 1)
                                        action-controller:*app-package*)))))
     ,@code))


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
