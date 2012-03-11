(in-package :asdf)

(defsystem pvm
  :name "pvm"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "A vitrual machine for psil"
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "env")
               (:file "op")
               (:file "pvm")))
