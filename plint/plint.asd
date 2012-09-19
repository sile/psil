(in-package :asdf)

(defsystem plint
  :name "plint"
  :version "0.0.1"
  
  :serial t
  :components ((:file "package")
               (:file "plint")))
