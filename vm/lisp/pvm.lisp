(in-package :pvm)

  
(defun execute (bytecode-stream)
  (exec.execute (empty-env bytecode-stream)))

(defun execute-from-list (bytecode-list &aux (tmppath "/tmp/pvm.bc.tmp"))
  (with-open-file (out tmppath :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
    (loop FOR x IN bytecode-list
          DO (write-byte x out)))
  (execute-from-file tmppath))

(defun execute-from-file (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (execute in)))