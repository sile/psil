(in-package :pvm)

(defun exec.execute (env)
  (loop UNTIL ($eos env)
        FOR op = ($read-op env)
    DO
    (op.call op env)
    FINALLY
    (return env)))
