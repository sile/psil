(defpackage :psil.op
  (:use :common-lisp :psil.bytecode)
  (:export read-op

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
    (:i.add (values #'@i.add 2))
    (:i.sub (values #'@i.sub 2))
    (:i.mul (values #'@i.mul 2))
    (:i.div (values #'@i.div 2))
    (:i.mod (values #'@i.mod 2))
    (:int (values #'@int 0 t))
    (:car (values #'@car 1))
    (:cdr (values #'@cdr 1))
    (:cons (values #'@cons 2))
    (:jump (values #'@jump 1 t))
    (:when.jump (values #'@when.jump 2 t))
    (:lambda (values #'@lambda 2 t))
    (:invoke (values #'@invoke 1 t))
    (:dup (values #'@dup 1))
    (:pop (values #'@pop 1))

    (:string (values #'@string 0 t))
    (:make-string (values #'@make-string 1))
;;    (:array (values #'@array 0 t))
    (:make-array (values #'@make-array 1))
    ))

(defun read-uint (in)
  (loop FOR i FROM 3 DOWNTO 0
        SUM (ash (read-byte in) (* i 8))))

(defun read-int (in)
  (let ((n (read-uint in)))
    (if (< n #x80000000)
        n
      (- n #x100000000))))

(defun read-octets (n in)
  (coerce (loop REPEAT n 
                COLLECT (read-byte in))
          '(vector (unsigned-byte 8))))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest args)
    (intern (format nil "~{~a~}" args))))

(defmacro def-binary-op (name args type raw-op &optional (coerce 'identity))
  `(defun ,name ,args
     (declare (,type ,@args))
     (,type (,coerce 
             (,raw-op ,@(mapcar (lambda (a)
                                  `(,(symb type '-value) ,a))
                                (reverse args)))))))

;;;;
(def-binary-op @i.add (n2 n1) %int +)
(def-binary-op @i.sub (n2 n1) %int -)
(def-binary-op @i.mul (n2 n1) %int *)
(def-binary-op @i.div (n2 n1) %int / floor)
(def-binary-op @i.mod (n2 n1) %int mod)

(defun @int (in stack)
  (cons (%int (read-int in)) stack))

;;;;
(defun @cons (x1 x2) (%cons x1 x2))
(defun @car (cs1) (%cons-car cs1))
(defun @cdr (cs1) (%cons-cdr cs1))

;;;;
;; TODO: 遷移先はtagで指定するようにする
(defun @jump (n1 in stack)
  (declare (%int n1))
  (let ((pos (file-position in)))
    (file-position in (+ pos (%int-value n1))))
  stack)

(defun @when.jump (n1 b1 in stack)
  (declare (%int n1 b1))
  (if (/= (%int-value b1) 0)
      (@jump n1 in stack)
    stack))

(defun @lambda (n1 n2 in stack)
  (declare (%int n1 n2))
  (cons (%lambda (read-octets (%int-value n1) in)
                 (loop REPEAT (%int-value n2) COLLECT (pop stack)))
        stack))

(defmacro with-input-from-octets ((in octets) &body body)
  (let ((path (gensym))
        (out (gensym)))
    `(let ((,path (format nil "/tmp/~a" (gentemp))))
       (with-open-file (,out ,path :direction :output
                                   :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
         (write-sequence ,octets ,out))
       (unwind-protect
         (with-open-file (,in ,path :element-type '(unsigned-byte 8))
           ,@body)
         (delete-file ,path)))))

(defun @invoke (fn1 in stack)
  (declare (ignore in)
           (%lambda fn1))
  (let ((body (%lambda-body fn1))
        (closure-stack (%lambda-stack fn1)))
    (with-input-from-octets (in body)
      (psil.bytecode-executor:execute in (append closure-stack stack)))))

(defun @dup (x1)
  (declare (%root x1))
  (list x1 x1))

(defun @pop (x1)
  (declare (ignore x1))
  '())

(defun @string (in stack)
  (let* ((len (read-int in))
         (octets (read-octets len in))
         (s (%string 0)))
    (setf (%string-octets s) octets)
    (cons s stack)))

(defun @make-string (n1)
  (declare (%int n1))
  (%string (%int-value n1)))

(defun @make-array (n1)
  (declare (%int n1))
  (%array (%int-value n1)))
