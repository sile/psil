(in-package :asdf)

(defsystem plc
  :name "plc"
  :version "0.0.1"
  :serial t
  :depends-on (:pvm)
  :components ((:file "package")
               (:file "util")
               (:file "bc-table")
               (:file "compile")
               (:file "plc")))
