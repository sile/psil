(defpackage pvm-compile
  (:use :common-lisp)
  (:nicknames :pvmc)
  (:export compile-to-file))
(in-package :pvm-compile)

(defparameter *op-list*
  '((1 :int)
    (2 :add)
    (3 :sub)
    (4 :mul)
    (5 :div)
    (6 :mod)
    (7 :eql)
    (8 :less)

    (9 :dup)
    (10 :drop)
    (11 :swap)
    (12 :over)
    (13 :rot)

    (14 :rpush)
    (15 :rpop)
    (16 :rcopy)
    
    (17 :jump)
    (18 :jump-if)
    (19 :call)
    (20 :return)))

(defun int-to-bytes (n)
  (loop FOR i FROM 0 BELOW 4
        COLLECT (ldb (byte 8 (* i 8)) n)))

(defun opcode (op)
  (assert #1=(find op *op-list* :key #'second))
  (first #1#))

(defun compile-to-bytecodes (codes)
  (loop WITH unresolves = '()
        WITH labels = '()
        FOR code IN codes
        FOR pos = (length tmps)
    APPEND
    (etypecase code
      (integer `(,(opcode :int) ,@(int-to-bytes code)))
      (keyword (list (opcode code)))
      (symbol (push `(,code ,pos) labels)
              '())
      (cons (ecase (first code)
              (:addr (push `(,(second code) ,(1+ pos)) unresolves)
                     `(,(opcode :int) 0 0 0 0)))))
    INTO tmps
    FINALLY
    (let ((bytecodes (coerce tmps 'vector)))
      (loop FOR (label offset) IN unresolves
            FOR label-addr = (second (assoc label labels))
        DO
        (setf (subseq bytecodes offset (+ offset 4)) (int-to-bytes label-addr)))

      (return bytecodes))))

(defun compile-to-file (filepath assembly-codes)
  (let ((bytecodes (compile-to-bytecodes assembly-codes)))
    (with-open-file (out filepath 
                         :direction :output
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8))
      (write-sequence bytecodes out)))
  t)
