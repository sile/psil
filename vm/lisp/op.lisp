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
  (funcall (symb "__" (op-name op)) env))

;; [整数系]
(defun __int (env) (@push @read-int))
(defun __i.add (env) (@push (+ @pop @pop)))
(defun __i.sub (env) (@push (+ (- @pop) @pop)))
(defun __i.mul (env) (@push (* @pop @pop)))
(defun __i.div (env &aux (n @pop)) (@push (floor (/ @pop n))))
(defun __i.mod (env &aux (n @pop)) (@push (mod @pop n)))
(defun __i.ash (env &aux (n @pop)) (@push (ash @pop n)))
(defun __i.bit-and (env) (@push (logand @pop @pop)))
(defun __i.bit-or (env) (@push (logior @pop @pop)))
(defun __i.bit-xor (env) (@push (logxor @pop @pop)))

;; [真偽系] 0=false, non-0=true
(defun __and (env) (__i.bit-and env))
(defun __or (env) (__i.bit-or env))
(defun __not (env) (@push (if (zerop @pop) 1 0)))
(defun __eq (env) (@push (if (= @pop @pop) 1 0)))
(defun __i.= (env) (@push (if (= @pop @pop) 1 0)))
(defun __i.< (env) (@push (if (< @pop @pop) 1 0)))
(defun __i.<=(env) (@push (if (<= @pop @pop) 1 0)))
(defun __i.> (env) (@push (if (> @pop @pop) 1 0)))
(defun __i.>=(env) (@push (if (>= @pop @pop) 1 0)))
(defun __i.!=(env) (@push (if (/= @pop @pop) 1 0)))

;; [データスタック系]
(defun __d.dup (env) (@push @head))
(defun __d.swap (env) @swap)
(defun __d.over (env) (@push (@ref 1)))
(defun __d.rot (env) ($rot env))
(defun __d.drop (env) @pop)


#|
[遷移系]
jump
jump-if
label
call
return

[データスタック系]
d.dup
d.swap
d.over
d.rot
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
