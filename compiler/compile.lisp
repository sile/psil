(in-package :pc)

(defun cp (ast &aux (*quote?* nil))
  (flatten (list (init-symtable)
                 (init-built-in-fun)
                 (from-object ast))))
   
