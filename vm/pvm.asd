(in-package :asdf)

(defsystem :pvm
  :name "pvm"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "internal/package")
               (:file "internal/pvmi")
               (:file "bytecode/package")
               (:file "bytecode/util")
               (:file "bytecode/reader")
               (:file "bytecode/writer")
               (:file "executor/package")
               (:file "executor/stream")
               (:file "executor/stack")
               (:file "executor/symbol-table")
               (:file "executor/environment")
               (:file "executor/instruction")
               (:file "executor/native-function")
               (:file "executor/executor")
               (:file "pvm")))
