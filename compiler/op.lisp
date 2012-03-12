(in-package :pc)

(defstruct (op (:constructor op (name code)))
  (name t :type keyword)
  (code 0 :type fixnum))

(defparameter *ops*
  (list (op :int 0)
        (op :i.add 1)
        (op :i.sub 2)
        (op :i.mul 3)
        (op :i.div 4)
        (op :i.mod 5)
        (op :i.ash 6)
        (op :i.bit-and 7)
        (op :i.bit-or 8)
        (op :i.bit-xor 9)

        (op :and 100)
        (op :or 101)
        (op :not 102)
        (op :eq 103)
        (op :i.= 104)
        (op :i.< 105)
        (op :i.<= 106)
        (op :i.> 107)
        (op :i.>= 108)
        (op :i.!= 109)
        
        (op :jump 200)
        (op :jump-if 201)
        (op :label 202)
        (op :call 203)
        (op :return 204)
        
        (op :d.dup 300)
        (op :d.swap 301)
        (op :d.over 302)
        (op :d.rot 303)
        (op :d.drop 304)
        
        (op :r.> 400)
        (op :r.< 401)
        (op :r.copy 402)
        
        (op :m.alloc 500)
        (op :m.free 501)
        (op :m.ref 502)
        (op :m.set 503)
        
        (op :variable 700)
        (op :getval 701)
        (op :setval 702)
        
        (op :c.print 800)
        ))

(defun name=>op (name)
  (a.if (find name *ops* :key #'op-name)
      it
    (error "undefined op name: ~a" name)))

;; to bytes
(defun $ (name)
  (short-to-bytes (op-code (name=>op name))))

  