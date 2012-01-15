(progn
  ;; open/close
  ((lambda (n)
     (write-byte 104 n)
     (close n))
   (open "/tmp/abc.txt" (+ o-creat o-rdwr)))

  ;; fib
  (set-symbol-value 'defun
    (lambda-macro (fn-name args body)
                  (list 'set-symbol-value (list 'quote fn-name)
                        (list 'lambda args body))))

  (defun fib (n)
    (if (< n 2)
        n
      (+ (fib (- n 2)) (fib (- n 1)))))

  (list (fib 21)
        (fib 22)
        (fib 23)))