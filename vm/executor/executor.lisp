(in-package :pvme)

(defparameter *loaded-bc-list* '())

(defun execute-op (op)
  op)

(defun execute (in)
  (with-new-env (in) 
    (loop UNTIL (eos? in)
          FOR op = (read-op in)
          DO (execute-op op))))

(defun load-bc (filepath)
  (let ((bc (pvm-bc:read-from-file filepath)))
    (push (list filepath bc) *loaded-bc-list*)
    (execute (make-code-stream (pvm-bc:bc-codes bc)))
    *loaded-bc-list*))

(defun init ()
  (setf *env* (make-env :stack (create-stack)
                        :symbols (create-symbol-table)
                        :code-stream (make-code-stream (make-array 0 :element-type 'octet)))
        *loaded-bc-list* '())
  (load-bc "/tmp/core.bc")
  t)
