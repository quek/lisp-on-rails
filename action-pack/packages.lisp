(defpackage :action-controller
    (:use :common-lisp :anaphora :active-support)
  (:export #:*routes*
           #:*routes-package*
           #:*controller*
           #:*app-package*
           #:connect))


(defpackage :action-view
    (:use :common-lisp :anaphora :active-support)
  (:export #:load-html
           #:compile-html-file))
