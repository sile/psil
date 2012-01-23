(in-package :psil.bytecode-executor)

(defun eos? (stream)
  (null (listen stream)))

(defun execute (bytecode-stream &optional stack)
  (macrolet ((call (op arity)
               `(if (null look-ahead-mode)
                    (funcall ,op ,@(loop REPEAT arity COLLECT '(pop stack)))
                  (funcall ,op ,@(loop REPEAT arity COLLECT '(pop stack)) bytecode-stream stack))))
    (multiple-value-bind (op arity look-ahead-mode)
                         (psil.op:read-op bytecode-stream)
      (let* ((result (ecase arity
                       (0 (call op 0))
                       (1 (call op 1))
                       (2 (call op 2))
                       (3 (call op 3))
                       (4 (call op 4))
                       (5 (call op 5))))
             (stack (if (null look-ahead-mode)
                        (if (listp result) ;; XXX:
                            (append result stack)
                          (cons result stack))
                      result)))
        (if (eos? bytecode-stream)
            stack
          (execute bytecode-stream stack))))))
