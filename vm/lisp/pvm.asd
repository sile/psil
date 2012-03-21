(in-package :asdf)

(defsystem pvm
  :name "pvm"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "VM"
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "ins")
               (:file "environment")
               (:file "bc-read")
               (:file "pvm")))
