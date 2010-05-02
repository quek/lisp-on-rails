(in-package :blog)


(require :lack-hunchentoot)

(lack:run (make-instance 'lack-hunchentoot:hunchentoot-web-server)
          (make-instance 'action-pack::dispatcher))