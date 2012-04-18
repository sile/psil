(in-package :asdf)

(defsystem plc
  :name "plc"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "plc")))