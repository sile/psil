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

      (16  :>r @>r)
      (17  :r> @r>)
      (18  :r@ @r@)
      (19  :r@3 @r@3)

      (20 :jump @jump)
      (21 :jump.if @jump.if)
      (22 :rel.jump @rel.jump)
      (23 :rel.jump.if @rel.jump.if)
      (24 :call @call)
      (25 :return @return)

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

(defun rpush (x env)
  (push x (pvm.env:return-stack env))
  env)

(defun rpop (env)
  (pop (pvm.env:return-stack env)))

(defun rhead (env)
  (car (pvm.env:return-stack env)))

(define-symbol-macro /pop (spop env))
(define-symbol-macro /head (shead env))
(define-symbol-macro /second (ssecond env))
(defmacro /push (x) `(spush ,x env))

(define-symbol-macro /i.pop (%int-value /pop))
(defmacro /i.push (x) `(/push (%int ,x)))

(define-symbol-macro /r.pop (rpop env))
(define-symbol-macro /r.head (rhead env))
(defmacro /r.push (x) `(rpush ,x env))

(define-symbol-macro /pc (pvm.env:pc env))

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

;;;
(defun @dup (env) (/push /head))
(defun @drop (env) /pop env)
(defun @over (env) (/push /second))
(defun @swap (env) (let ((stack (pvm.env:stack env)))
                     (rotatef (first stack) (second stack))
                     env))
(defun @rot (env) (let ((stack (pvm.env:stack env)))
                    (rotatef (third stack) (second stack) (first stack))
                    env))

;;;
(defun @>r (env) (/r.push /pop))
(defun @r> (env) (/push /r.pop))
(defun @r@ (env) (/push /r.head))
(defun @r@3 (env) (let ((rstack (pvm.env:return-stack env)))
                    (/push (third rstack))))

;;;
(defun @jump (env)
  (setf /pc /i.pop)
  env)

(defun @jump.if (env)
  (let ((new-pc /i.pop))
    (unless (= /i.pop 0)
      (setf /pc new-pc)))
  env)

(defun @rel.jump (env)
  (incf /pc /i.pop)
  env)

(defun @rel.jump.if (env)
  (let ((offset /i.pop))
    (unless (= /i.pop 0)
      (incf /pc offset))
    env))

(defun @call (env)
  (/r.push /pc)
  (@jump env))

(defun @return (env)
  (let ((caller-pc (%int-value /r.pop)))
    (setf /pc caller-pc)
    env))
