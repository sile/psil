;;;; 標準ライブラリ (よりも低レベルかも)

;; defun
(set-symbol-value 'defun
  (lambda-macro (fn-name args &rest body)
                (list 'set-symbol-value (list 'quote fn-name)
                      (cons 'lambda (cons args body)))))

(defun fib (n)
  (if (< n 2)
      n
    (+ (fib (- n 2)) (fib (- n 1)))))

(list (fib 21)
      (fib 22)
      (fib 23))
