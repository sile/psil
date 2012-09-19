(in-package :asdf)

(defsystem plint
  :name "plint"
  :version "0.0.1"
  
  :serial t
  :components ((:file "package")
               (:file "parser/package")
               (:file "parser/util")
               (:file "parser/parser")
               (:file "plint")))
