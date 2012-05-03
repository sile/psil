(in-package :asdf)

(defsystem plc
  :name "plc"
  :version "0.0.1"
  :author "Takeru Ohta"

  :serial t
  :components ((:file "package")
               (:file "parser/package")
               (:file "parser/util")
               (:file "parser/parse")
               (:file "compiler/package")
               (:file "plc")))