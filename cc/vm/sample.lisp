(load "pvm-compile")

;; addition
(pvmc:compile-to-file
 "/tmp/add.bc"
 '(10 20 :add))  ; 10 + 20

;; Fibonacci number
(pvmc:compile-to-file
 "/tmp/fib.bc"
 '(
   25 ; argument
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
