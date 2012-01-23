(require :asdf)

(defsystem :psil
  :name "psil"
  :version "0.1.0"
  :author "Takeru Ohta"
  :description "A dialect of lisp"

  :serial t
  :components ((:file "package")
               
               ;; bytecode
               (:file "bytecode")
               (:file "op")
               (:file "bytecode-executor")
               (:file "compile")
               (:file "psil")))
