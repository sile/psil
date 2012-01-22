(in-package :psil)

(defun read (in)
  (psil.reader:read in))

(defun read-from-string (str)
  (with-input-from-string (in str)
    (read in)))

(defun eval (exp &key (env (psil.evaluator::null-env)))
  (psil.evaluator:eval exp env))

(defun init-env ()
  (psil.evaluator:init-env))
