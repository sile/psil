(load "pvm-compile")

(pvmc:compile-to-file
 "/tmp/fib.bc"
 '(
   25
   (:addr fib-beg) :call
   (:addr finish)  :jump
   
   fib-beg
   :dup 2  :less (:addr fib-end) :jump-if
   :dup 2  :sub  (:addr fib-beg) :call
   :swap 1 :sub  (:addr fib-beg) :call
   :add
   fib-end
   :return
   
   finish
   ))
