(defpackage psil.bytecode-executor
  (:use :common-lisp)
  (:export execute))
(in-package :psil.bytecode-executor)

(defun eos? (stream)
  (null (listen stream)))

(defun execute (bytecode-stream &optional stack)
  (macrolet ((call (op arity)
               `(funcall ,op ,@(loop REPEAT arity COLLECT '(pop stack)))))
    (multiple-value-bind (op operand arity) 
                         (psil.op:read-op bytecode-stream)
      
      (let* ((stack (nconc (loop REPEAT operand
                                 COLLECT(psil.op:read-operand bytecode-stream))
                           stack))
             (result (ecase arity
                       (0 (call op 0))
                       (1 (call op 1))
                       (2 (call op 2))
                       (3 (call op 3))
                       (4 (call op 4))
                       (5 (call op 5))))
             (stack (cons result stack)))
        (if (eos? bytecode-stream)
            stack
          (execute bytecode-stream stack))))))
