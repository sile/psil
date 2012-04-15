(in-package :asdf)

(defsystem :pvm
  :name "pvm"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "internal/package")
               (:file "internal/pvmi")
               (:file "bytecode/package")
               (:file "bytecode/reader")
               (:file "pvm")))
