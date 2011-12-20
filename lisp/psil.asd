(require :asdf)

(defsystem :psil
  :name "psil"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "A dialect of lisp"

  :serial t
  :components ((:file "package")
               (:file "reader")
               (:file "evaluator")
               (:file "psil")))
