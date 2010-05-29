(defpackage :action-controller
    (:use :common-lisp :anaphora :active-support :lack)
  (:export #:*routes*
           #:*routes-package*
           #:connect))


(defpackage :action-view
    (:use :common-lisp :anaphora :active-support :lack)
  (:export #:load-html
           #:compile-html-file))
