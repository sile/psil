(in-package :asdf)

(defsystem pbc-core
  :name "pbc-core"
  :version "0.0.1"
  :description "Core bytecode generator for psil"
  :serial t
  :depends-on (:pvm)
  :components ((:file "package")
               (:file "compiler")
               (:file "pbc-core")))
