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
    (:i.= (values #'@i.= 2))
    (:i.< (values #'@i.< 2))
    (:int (values #'@int 0 t))
    (:car (values #'@car 1))
    (:cdr (values #'@cdr 1))
    (:cons (values #'@cons 2))
    (:jump (values #'@jump 1 t))
    (:when.jump (values #'@when.jump 2 t))
    (:abs-jump (values #'@abs-jump 1 t))
    (:lambda (values #'@lambda 2 t))
;;    (:return (values #'@return 0 t t))
    (:rpush (values #'@rpush 1 t t))
    (:rpop (values #'@rpop 0 t t))

    (:invoke (values #'@invoke 1 t t))
    (:dup (values #'@dup 1))
    (:pop (values #'@pop 1))

    (:string (values #'@string 0 t))
    (:make-string (values #'@make-string 1))
;;    (:array (values #'@array 0 t))
    (:make-array (values #'@make-array 1))
    (:ref (values #'@ref 2))
    (:ref! (values #'@ref! 3))

    (:make-symbol (values #'@make-symbol 2))
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

(defun bool-to-int (b)
  (if b 1 0))

(def-binary-op @i.= (n2 n1) %int = bool-to-int)
(def-binary-op @i.< (n2 n1) %int < bool-to-int)

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

(defun @abs-jump (n1 in stack)
  (declare (%int n1))
  (file-position in (%int-value n1))
  stack)

(defun @when.jump (n1 b1 in stack)
  (declare (%int n1 b1))
  (if (/= (%int-value b1) 0)
      (@jump n1 in stack)
    stack))

(defun @lambda (n1 n2 in stack)
  (declare (%int n1 n2))
  (let ((fn (%lambda (file-position in)
                     (loop REPEAT (%int-value n2) COLLECT (pop stack)))))
    (read-octets (%int-value n1) in) ; discard
  (cons fn stack)))

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

(defun @invoke (fn1 in stack rstack)
  (declare (%lambda fn1))
  (let ((body-pos (%int (%lambda-body fn1)))
        (closure-stack (%lambda-stack fn1))
        (return-pos (file-position in)))
    (values (@abs-jump body-pos in (append closure-stack stack))
            (cons (%int return-pos) rstack))))

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

(defun @ref (n1 a1)
  (declare (%int n1)
           ((or %array %string) a1))
  (typecase a1
    (%array (aref (%array-data a1) (%int-value n1)))
    (%string (%int (aref (%string-octets a1) (%int-value n1))))))

(defun @ref! (x1 n1 a1)
  (declare (%int n1)
           ((or %array %string) a1)
           (%root x1))
  (typecase a1
    (%array (setf (aref (%array-data a1) (%int-value n1)) x1))
    (%string 
     (locally
      (declare (%int x1))
      (setf (aref (%string-octets a1) (%int-value n1)) (%int-value x1)))))
  a1)

(defun @make-symbol (value name)
  (declare (%root value)
           (%string name))
  (%symbol name value))

(defun @return (in stack rstack)
  (declare (ignore in))
  (let ((addr (pop rstack)))
    (values (cons addr stack) rstack)))

(defun @rpush (x1 in stack rstack)
  (declare (ignore in))
  (values stack (cons x1 rstack)))

(defun @rpop (in stack rstack)
  (declare (ignore in))
  (let ((x1 (pop rstack)))
    (values (cons x1 stack) rstack)))
