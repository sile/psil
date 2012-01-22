(defpackage psil.bytecode
  (:use :common-lisp)
  (:export op.code->sym
           %root
           %int %int-value
           %cons %cons-car %cons-cdr
           ))
(in-package :psil.bytecode)

#|
### TYPE ###
[name] [tag] [value]
int    1    [(signed-byte 32)]
cons   2    [car cdr]

### OP ###
[name] [code] [in-stack] [operand] [out-stack] [description]
i.add  0      n1 n2                n3           n1+n2
i.sub  1      n1 n2                n3           n1-n1
i.mul  2      n1 n2                n3           n1*n2
i.div  3      n1 n2                n3           n1/n2
i.mod  4      n1 n2                n3           n1 mod n2
int    5                  r1       n1           

car    6      cs1                  x1           (car cs1)
cdr    7      cs1                  x2           (cdr cs1)
cons   8      x1 x2                cs1          (cons x1 x2)
|#

(defun op.code->sym (op)
  (ecase op
    (0 :i.add)
    (1 :i.sub)
    (2 :i.mul)
    (3 :i.div)
    (4 :i.mod)
    (5 :int)
    (6 :car)
    (7 :cdr)
    (8 :cons)))

(defstruct %root
  (tag 0 :type fixnum))

(defstruct (%int (:include %root)
                 (:constructor %int (value &aux (tag 1))))
  (value 0 :type (unsigned-byte 32)))
(defmethod print-object ((o %int) stream)
  (print-unreadable-object (o stream)
    (format stream "INT ~a" (%int-value o))))

(defstruct (%cons (:include %root)
                  (:constructor %cons (car cdr &aux (tag 2))))
  (car t :type %root)
  (cdr t :type %root))
(defmethod print-object ((o %cons) stream)
  (print-unreadable-object (o stream)
    (format stream "CONS ~a ~a" (%cons-car o) (%cons-cdr o))))



