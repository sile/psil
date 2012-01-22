(defpackage :psil.op
  (:use :common-lisp :psil.bytecode)
  (:export read-op
           read-operand

           @i.add
           @i.sub
           @i.mul
           @i.div
           @i.mod
           @int

           @car
           @cdr
           @cons
           ))
(in-package :psil.op)

(defun read-op (in)
  (ecase (op.code->sym (read-byte in))
    (:i.add (values #'@i.add 0 2))
    (:i.sub (values #'@i.sub 0 2))
    (:i.mul (values #'@i.mul 0 2))
    (:i.div (values #'@i.div 0 2))
    (:i.mod (values #'@i.mod 0 2))
    (:int (values #'@int 1 1))
    (:car (values #'@car 0 1))
    (:cdr (values #'@cdr 0 1))
    (:cons (values #'@cons 0 2))))

(defun read-operand (in)
  (loop FOR i FROM 3 DOWNTO 0
        SUM (ash (read-byte in) (* i 8))))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest args)
    (intern (format nil "~{~a~}" args))))

(defmacro def-binary-op (name args type raw-op &optional (coerce 'identity))
  `(defun ,name ,args
     (declare (,type ,@args))
     (,type (,coerce 
             (,raw-op ,@(mapcar (lambda (a)
                                  `(,(symb type '-value) ,a))
                                args))))))

;;;;
(def-binary-op @i.add (n1 n2) %int +)
(def-binary-op @i.sub (n1 n2) %int -)
(def-binary-op @i.mul (n1 n2) %int *)
(def-binary-op @i.div (n1 n2) %int / floor)
(def-binary-op @i.mod (n1 n2) %int mod)

(defun @int (r1)
  (declare ((unsigned-byte 32) r1))
  (%int r1))

;;;;
(defun @cons (x1 x2) (%cons x1 x2))
(defun @car (cs1) (%cons-car cs1))
(defun @cdr (cs1) (%cons-cdr cs1))
