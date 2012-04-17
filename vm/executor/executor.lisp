(in-package :pvme)

(defparameter *loaded-bc-list* '())

(defun load-bc (filepath)
  (let ((bc (pvm-bc:read-from-file filepath)))
    (push (list filepath bc) *loaded-bc-list*)
    (execute (make-code-stream (pvm-bc:bc-codes bc)))))

(defun init-natives ()
  (loop FOR (sym val) IN *natives*
        DO (set-symbol-value +symbols+ sym val)))

(defun init ()
  (setf *env* (make-env :stack (create-stack)
                        :symbols (create-symbol-table)
                        :code-stream (make-code-stream (make-array 0 :element-type 'octet)))
        *loaded-bc-list* '())
  (init-natives)
  t)

(defun execute (in &key (initialize t))
  (when initialize
    (init))
  (with-new-env (in) 
    (loop UNTIL (eos? +in+)
          FOR op = (read-op +in+)
          DO (execute-op op))
    (env-stack *env*)))

(defun execute-from-stream (in &key (initialize t))
  (execute (make-code-stream (pvm-bc:bc-codes (pvm-bc:read-from-stream in)))
           :initialize initialize))

(defun execute-from-file (path &key (initialize t))
  (execute (make-code-stream (pvm-bc:bc-codes (pvm-bc:read-from-file path)))
           :initialize initialize))
