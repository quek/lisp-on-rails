(in-package :blog)

(def-record comment
  (:belongs-to post))
