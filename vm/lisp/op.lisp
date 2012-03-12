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
        
        (op :m.alloc 500)
        (op :m.free 501)
        (op :m.ref 502)
        (op :m.set 503)
        
        (op :variable 700)
        (op :getval 701)
        (op :setval 702)
        
        (op :c.print 800)
        (op :dstack.print 801) ; for debug
        (op :rstack.print 802) ; for debug
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

;; [リターンスタック系]
(defun __r.> (env) (@r.push @pop))
(defun __r.< (env) (@push @r.pop))
(defun __r.copy (env) (@push @r.head))

;; [ヒープ系]
(defun __m.alloc (env) 
  (@push ($h.register env (make-array @pop :element-type 'int4))))

(defun __m.free (env)
  ($h.deregister env @pop))

(defun __m.ref (env)
  (@push ($m.ref env @pop @pop)))

(defun __m.set (env)
  ($m.set env @pop @pop @pop))

;; [遷移系]
(defun __jump (env)
  ($jump env ($label-addr env @pop)))

(defun __jump-if (env &aux (pos ($label-addr env @pop)))
  (unless (zerop @pop)
    ($jump env pos)))

(defun __label (env)
  @read-int) ;; 実行時には何もしない

(defun __call (env)
  (@r.push @pc)
  (__jump env))

(defun __return (env)
  ($jump env @r.pop))

;; [グルーバル変数系]
(defun __variable (env)
  ($v.register env @read-int))

(defun __getval (env)
  (@push ($v.get env @pop)))

(defun __setval (env)
  ($v.set env @pop @pop))

;; [組み込み関数系]
(defun __c.print (env)
  (write-byte @pop *standard-output*))

(defun __dstack.print (env)
  (format t "~&~a~%" (env-stack env)))

(defun __rstack.print (env)
  (format t "~&~a~%" (env-rstack env)))
