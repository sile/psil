
(defun fib (n)
  (if (< n 2)
      n
    (+ (fib (- n 2)) (fib (- n 1)))))

(list (fib 21)
      (fib 22)
      (fib 23))
