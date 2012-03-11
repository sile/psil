(in-package :pvm)

(defun exec.preprocess (env)
  (loop WITH op-label = (op-code (name=>op :label))
        UNTIL ($eos env)
        FOR op = ($read-op env)
        WHEN (= op op-label)
    DO
    ($add-label env))
  ($jump env 0))

(defun exec.execute (env)
  (exec.preprocess env)
  (loop UNTIL ($eos env)
        FOR op = ($read-op env)
    DO
    (op.call op env)
    FINALLY
    (return env)))
