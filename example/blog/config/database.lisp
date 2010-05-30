((:development
  :connection-spec ("localhost" "blog_development" "root" "")
  :database-type :mysql)

 (:test
  :connection-spec ("localhost" "blog_test" "root" "")
  :database-type :mysql)

 (:production
  :connection-spec ("localhost" "blog_production" "root" "")
  :database-type :mysql))
