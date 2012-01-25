(defpackage psil-vm.op
  (:use :common-lisp :psil-vm :psil-vm.type)
  (:nicknames :pvm.op)
  (:export *op-list*
           read-op))
(in-package :psil-vm.op)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *op-list*
    '(
      (1   :int @int)
      (2   :short @short)
      (3   :byte @byte)

      (4   :i.add @i.add)
      (5   :i.sub @i.sub)
      (6   :i.mul @i.mul)
      (7   :i.div @i.div)
      (8   :i.mod @i.mod)
      (9   :i.= @i.=)
      (10  :i.< @i.<)

      (11  :dup @dup)
      (12  :drop @drop)
      (13  :swap @swap)
      (14  :over @over)
      (15  :rot @rot)
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

(defun spop (env)
  (pop (pvm.env:stack env)))

(defun shead (env)
  (car (pvm.env:stack env)))

(defun ssecond (env)
  (second (pvm.env:stack env)))

(define-symbol-macro /pop (spop env))
(define-symbol-macro /head (shead env))
(define-symbol-macro /second (ssecond env))
(defmacro /push (x) `(spush ,x env))

(define-symbol-macro /i.pop (%int-value /pop))
(defmacro /i.push (x) `(/push (%int ,x)))

(defun b2i (bool) (if bool 1 0))

;;;;;;;;;;
(defun @int (env)
  (/i.push (pvm.stream:read-int32 env)))

(defun @short (env)
  (/i.push (pvm.stream:read-int16 env)))

(defun @byte (env)
  (/i.push (pvm.stream:read-int8 env)))

(defun @i.add (env) (/i.push (+ /i.pop /i.pop)))
(defun @i.sub (env) (/i.push (+ (- /i.pop) /i.pop)))
(defun @i.mul (env) (/i.push (* /i.pop /i.pop)))
(defun @i.div (env) (let ((n /i.pop))
                      (/i.push (truncate /i.pop n))))
(defun @i.= (env) (/i.push (b2i (= /i.pop /i.pop))))
(defun @i.< (env) (/i.push (b2i (> /i.pop /i.pop))))

(defun @dup (env) (/push /head))
(defun @drop (env) /pop env)
(defun @over (env) (/push /second))
(defun @swap (env) (let ((stack (pvm.env:stack env)))
                     (rotatef (first stack) (second stack))
                     env))
(defun @rot (env) (let ((stack (pvm.env:stack env)))
                    (rotatef (third stack) (second stack) (first stack))
                    env))
