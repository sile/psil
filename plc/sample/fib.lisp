(begin
 (define fib (lambda (n)   ; comment
               (if (< n 2)
                   n
                 (+ (fib (- n 2)) (fib (- n 1))))))
 (fib 25))


   