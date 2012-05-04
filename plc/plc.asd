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
               (:file "compiler/util")
               (:file "compiler/instruction")
               (:file "compiler/bcobj")
               (:file "compiler/compile")
               (:file "compiler/plcc")
               (:file "plc")))