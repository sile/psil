(in-package :asdf)

(defsystem pvm
  :name "pvm"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "A vitrual machine for psil"
  :sertial t
  :components ((:file "package")
               (:file "pvm")))
