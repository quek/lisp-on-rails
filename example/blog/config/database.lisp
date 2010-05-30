(defparameter
    active-record::*database-settings*
  `((:development
     :adapter :mysql
     :encoding "utf-8"
     :host "localhost"
     :database "blog_development"
     :username "root"
     :password "")

    (:test
     :adapter :mysql
     :encoding "utf-8"
     :host "localhost"
     :database "blog_test"
     :username "root"
     :password "")

    (:production
     :adapter :mysql
     :encoding "utf-8"
     :host "localhost"
     :database "blog_prodution"
     :username "root"
     :password "")))
