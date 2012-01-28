(load "pvm-compile")

;; addition
(pvmc:compile-to-file
 "/tmp/add.bc"
 '(10 20 :add))  ; 10 + 20

;; Fibonacci number
(pvmc:compile-to-file
 "/tmp/fib.bc"
 '(
   35 ; argument
   (:addr fib-beg) :call ; fib(25)
   (:addr finish)  :jump
   
   fib-beg
   :dup 2  :less (:addr fib-end) :jump-if  ; if(n < 2) 
   :dup 2  :sub  (:addr fib-beg) :call     ; fib(n - 2)
   :swap 1 :sub  (:addr fib-beg) :call     ; fib(n - 1)
   :add
   fib-end
   :return
   
   finish
   ))

;; シンボルはアドレス参照用のラベル
;; (:addr シンボル)形式で参照可能
;; ※ アドレスはコンパイル時に解決される
(pvmc:compile-to-file
 "/tmp/jump.bc"
 '(10 10 :eql            ; n1 == n2 ?
   (:addr then) :jump-if ; 等しいなら then に移動
   else
   1 2     ; else: スタックに 1と2 を積む
   (:addr end) :jump
   then 
   3 4      ; then: スタックに 3と4 を積む
   end))
   
      
 '(10 20 :add))  ; 10 + 20