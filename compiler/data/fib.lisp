(progn
 (setval fib (lambda (n)
               (if ($i.<= n 1) n ($+ (fib ($- n 2)) (fib ($- n 1))))))
 (fib 10))
