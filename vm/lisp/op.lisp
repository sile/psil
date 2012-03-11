(in-package :pvm)

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
        (op :i.bsl 6)
        (op :i.bsr 7)
        (op :i.bit-and 8)
        (op :i.bit-or 9)
        (op :i.bit-xor 10)

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
        
        (op :d.ref 300)
        (op :d.dup 301)
        (op :d.swap 302)
        (op :d.over 303)
        (op :d.rot 304)
        (op :d.push 305)
        (op :d.pop 306)
        (op :d.drop 307)
        
        (op :r.> 400)
        (op :r.< 401)
        (op :r.copy 402)
        
        (op :m.alloc 400)
        (op :m.free 401)
        (op :m.i.ref 402)
        (op :m.i.set 403)
        
        (op :fun 500)
        (op :end 501)
        (op :native-fun 502)
        
        (op :constant 600)
        (op :variable 601)
        (op :getvar 602)
        (op :setvar 603)
        
        (op :i.print 700)

        (op :load-native-library 800)
        ))

(defun code=>op (code)
  (a.if (find code *ops* :key #'op-code)
      it
    (error "undefined op code: ~a" code)))

(defun name=>op (name)
  (a.if (find name *ops* :key #'op-name)
      it
    (error "undefined op name: ~a" name)))

(defun op.call (code env &aux (op (code=>op code)))
  (apply (symb "__" (op-name op)) env))

(defun __int (env)
  ($push env ($read-int env)))

#|
[整数系]
int
i.add
i.sub
i.mul
i.div
i.mod
i.bsl
i.bsr
i.bit-and
i.bit-or
i.bit-xor

[真偽系]
and
or
not
eq
i.=
i.<
i.<=
i.>
i.>=
i.!=

[遷移系]
jump
jump-if
label
call
return

[データスタック系]
d.ref
d.dup
d.swap
d.over
d.rot
d.push
d.pop
d.drop

[リターンスタック系]
r.>
r.<
r.copy

[ヒープ系]
m.alloc
m.free
m.i.ref
m.i.set

[関数系]
fun        ; (fun ...)
end
native-fun

[定数/変数系]
constant
variable
getvar
setvar

[組み込み関数系]
・・・

[その他]
load-native-library
|#
