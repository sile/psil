(defpackage psil.bytecode
  (:use :common-lisp)
  (:export op.code->sym
           op.sym->code
           %root
           %int %int-value
           %cons %cons-car %cons-cdr
           %lambda %lambda-body %lambda-stack
           %array %array-data
           %string %string-octets
           %symbol %symbol-name %symbol-value
           %refer %refer-value
           ))
(in-package :psil.bytecode)

#|
### TYPE ###
[name] [tag] [value]
int    1    [(signed-byte 32)]
cons   2    [car cdr]
lambda 3    [bytecodes]
array  4    [data]
string 5    [octets]
symbol 6    [name value]

refer  7    [pointer]
native 8    [...]

### OP ###
[name] [code] [in-stack] [operand] [out-stack] [description]
i.add  0      n1 n2                n3           n2+n1
i.sub  1      n1 n2                n3           n2-n1
i.mul  2      n1 n2                n3           n2*n1
i.div  3      n1 n2                n3           n2/n1
i.mod  4      n1 n2                n3           n2 mod n1
int    5                  r1       n1           

car    6      cs1                  x1           (car cs1)
cdr    7      cs1                  x2           (cdr cs1)
cons   8      x1 x2                cs1          (cons x1 x2)

jump   9      n1                                jump n1 bytes
when.jump 10  n1 b1                             jump n1 bytes when b1 != 0

lambda 11     n1 n2       x*       fn1          lambda ... n1(end) => fn1, n2 = number of closure value
invoke 12     fn1 x*               x*           fn1(...) => ...

dup    13     x1                   x1 x1
pop    14     x1                                discard

load   15
store  16

rpush  17   (return-stack)
rpop   18

string 19                    n1    s1           n1=length
make-string 20               
array  21
make-array 22

ref    23 
ref!   24

i.=    25
i.<    26

make-symbol 27

return 28
abs-jump 29

make-refer 30

|#

(defparameter *code-sym*
  '((0 :i.add)
    (1 :i.sub)
    (2 :i.mul)
    (3 :i.div)
    (4 :i.mod)
    (5 :int)
    (6 :car)
    (7 :cdr)
    (8 :cons)
    (9 :jump)
    (10 :when.jump)
    (11 :lambda)
    (12 :invoke)
    (13 :dup)
    (14 :pop)

    (17 :rpush)
    (18 :rpop)

    (19 :string)
    (20 :make-string)
    (21 :array)
    (22 :make-array)
    (23 :ref)
    (24 :ref!)

    (25 :i.=)
    (26 :i.<)

    (27 :make-symbol)
;;    (28 :return)
    (29 :abs-jump)
    
    (30 :make-refer)
    (15 :load)
    (16 :store)
    ))

(defun op.code->sym (op)
  (assert (member op *code-sym* :key #'first))
  (second (find op *code-sym* :key #'first)))

(defun op.sym->code (sym)
  (assert (member sym *code-sym* :key #'second))
  (first (find sym *code-sym* :key #'second)))

(defstruct %root
  (tag 0 :type fixnum))

(defstruct (%int (:include %root)
                 (:constructor %int (value &aux (tag 1))))
  (value 0 :type (signed-byte 32)))
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

(defstruct (%lambda (:include %root)
                    (:constructor %lambda (body &optional stack &aux (tag 3))))
  (stack '() :type list) ; for closure
  (body #() :type fixnum))
(defmethod print-object ((o %lambda) stream)
  (print-unreadable-object (o stream :identity t)
    (format stream "LAMBDA ~a" (%lambda-body o))))

(defstruct (%array (:include %root)
                   (:constructor %array (len &aux (tag 4) (data (make-array len)))))
  (data #() :type simple-array))
(defmethod print-object ((o %array) stream)
  (print-unreadable-object (o stream)
    (format stream "[~{~a~^ ~}]" (coerce (%array-data o) 'list))))

(defstruct (%string (:include %root)
                    (:constructor %string (len &aux (tag 5) 
                                               (octets (make-array len :element-type '(unsigned-byte 8))))))
  (octets #() :type (simple-array (unsigned-byte 8))))
(defmethod print-object ((o %string) stream)
  (print-unreadable-object (o stream)
    (format stream "~s" (sb-ext:octets-to-string (%string-octets o)))))

;; TODO: 多分廃止
(defstruct (%symbol (:include %root)
                    (:constructor %symbol (name value &aux (tag 6))))
  (name t :type %string)
  (value t :type %root))
(defmethod print-object ((o %symbol) stream)
  (print-unreadable-object (o stream)
    (format stream "SYMBOL ~a ~a" (%symbol-name o) (%symbol-value o))))

(defstruct (%refer (:include %root)
                   (:constructor %refer (value &aux (tag 7))))
  (value t :type %root))
(defmethod print-object ((o %refer) stream)
  (print-unreadable-object (o stream)
    (format stream "{~a}" (%refer-value o))))