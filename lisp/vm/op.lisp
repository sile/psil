(defpackage psil-vm.op
  (:use :common-lisp :psil-vm :psil-vm.type)
  (:nicknames :pvm.op)
  (:export read-op))
(in-package :psil-vm.op)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *op-list*
    '(
      (1 :int @int)
      (2 :short @short)
      (3 :byte @byte)
      )))

#.`(progn
     ,@(mapcar (lambda (op)
                 `(declaim (ftype (function (pvm.env:env) pvm.env:env) ,(third op))))
               *op-list*))

(defmacro get-op-fun (code)
  `(the (function (pvm.env:env) pvm.env:env)
        (ecase ,code
          ,@(mapcar (lambda (op)
                      `(,(first op) (function ,(third op))))
                    *op-list*))))

(defun read-op (env)
  (get-op-fun (pvm.stream:read-octet env)))

;;;;;;;;;;
(defun spush (x env)
  (push x (pvm.env:stack env))
  env)

;;;;;;;;;;
(defun @int (env)
  (spush (%int (pvm.stream:read-int32 env)) env))

(defun @short (env)
  (spush (%int (pvm.stream:read-int16 env)) env))

(defun @byte (env)
  (spush (%int (pvm.stream:read-int8 env)) env))