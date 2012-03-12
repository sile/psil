(in-package :pc)

(defun cp (ast &aux (*quote* nil))
  (flatten (list (init-symtable)
                 (from-object ast))))
   
