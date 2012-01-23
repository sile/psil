(defpackage psil.bytecode-executor
  (:use :common-lisp)
  (:export execute))
(in-package :psil.bytecode-executor)

(defun eos? (stream)
  (null (listen stream)))

(defun execute (bytecode-stream &optional stack rstack (psil.op:*heap* #()))
  (macrolet ((call (op arity)
               `(if (null look-ahead-mode)
                    (funcall ,op ,@(loop REPEAT arity COLLECT '(pop stack)))
                  (if (null use-rstack) ;; XXX: 複雑...
                      (funcall ,op ,@(loop REPEAT arity COLLECT '(pop stack)) bytecode-stream stack)
                    (funcall ,op ,@(loop REPEAT arity COLLECT '(pop stack)) bytecode-stream stack rstack)))))
    (multiple-value-bind (op arity look-ahead-mode use-rstack)
                         (psil.op:read-op bytecode-stream)
      (multiple-value-bind (result new-rstack)
                           (ecase arity
                             (0 (call op 0))
                             (1 (call op 1))
                             (2 (call op 2))
                             (3 (call op 3))
                             (4 (call op 4))
                             (5 (call op 5)))
        (let ((stack (if (null look-ahead-mode)
                         (if (listp result) ;; XXX:
                             (append result stack)
                           (cons result stack))
                       result)))
          (if (eos? bytecode-stream)
              stack
            (execute bytecode-stream stack (or new-rstack rstack) psil.op:*heap*)))))))
