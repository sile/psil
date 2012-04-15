(in-package :pvme)

(defparameter *loaded-bc-list* '())

(defun execute (in)
  (with-new-env (in) 
    (loop UNTIL (eos? in)
          FOR op = (read-op in)
          DO (execute-op op))
    (env-stack *env*)))

(defun load-bc (filepath)
  (let ((bc (pvm-bc:read-from-file filepath)))
    (push (list filepath bc) *loaded-bc-list*)
    (execute (make-code-stream (pvm-bc:bc-codes bc)))))

(defun init ()
  (setf *env* (make-env :stack (create-stack)
                        :symbols (create-symbol-table)
                        :code-stream (make-code-stream (make-array 0 :element-type 'octet)))
        *loaded-bc-list* '())
  #+C
  (load-bc "/tmp/core.bc")
  t)
