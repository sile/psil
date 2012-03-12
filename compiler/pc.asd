(in-package :asdf)

(defsystem pc
  :name "pc"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "A bytecode-compiler for psil"
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "op")
               (:file "symtable")
               (:file "object")
               (:file "parse")
               (:file "compile")
               (:file "pc")))
