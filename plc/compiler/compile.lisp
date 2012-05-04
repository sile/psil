(in-package :plcc)

(defmacro $ (&rest exps)
  `(flatten 
    (list ,@(loop FOR e IN exps
                  COLLECT (typecase e
                            (keyword `(ins ,e))
                            (t e))))))

(defun @int (exp) ($ :int (int-to-bytes exp)))
(defun @true () ($ :true))
(defun @false () ($ :false))

(defun compile-impl (exp)
  (etypecase exp
    (integer (@int exp))
    (symbol 
     (ecase exp
       (:|true| (@true))
       (:|false| (@false))
       ))))