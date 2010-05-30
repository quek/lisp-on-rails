(in-package :blog)

(def-record post
  (:has-many comments))
