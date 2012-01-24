(require :asdf)

(defsystem :psil-vm
  :name "psil-vm"
  :version "0.1.0"
  :author "Takeru Ohta"
  :description "A dialect of lisp"

  :serial t
  :components ((:file "package")
               (:file "environment")
               (:file "stream")
               (:file "op")
               (:file "executor")
               (:file "psil-vm")))
