(defpackage pvm.environment
  (:use :common-lisp :psil-vm)
  (:nicknames :pvm.env)
  (:export init
           pc
           bytecodes
           stack
           return-stack
           heap
           
           env))
(in-package :pvm.environment)

(defstruct env
  (pc             0 :type fixnum)  ; program counter
  (bytecodes    #() :type octets)
  (stack        '() :type list)
  (return-stack '() :type list)
  (heap         #() :type simple-vector))

(defun init (bytecodes &optional base-env)
  (if (null base-env)
      (make-env :bytecodes bytecodes)
    (progn (setf (env-pc base-env) 0
                 (env-bytecodes base-env) bytecodes)
           base-env)))

(defmacro pc (env) `(env-pc ,env))
(defmacro bytecodes (env) `(env-bytecodes ,env))
(defmacro stack (env) `(env-stack ,env))
(defmacro return-stack (env) `(env-return-stack ,env))
(defmacro heap (env) `(env-heap ,env))
