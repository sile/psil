;;;; 標準ライブラリ (よりも低レベルかも)

;; defmacro
(set-symbol-value 'defmacro
  (lambda-macro (fn-name args &rest body)
                (list 'set-symbol-value (list 'quote fn-name)
                      (cons 'lambda-macro (cons args body)))))

;; defun
(defmacro defun (fn-name args &rest body)
  (list 'set-symbol-value (list 'quote fn-name)
        (cons 'lambda (cons args body))))

;; setq
(defmacro setq (symbol value)
  (list 'set-symbol-value (list 'quote symbol) value))

;; mapcar 
#|
(defun mapcar (fn list)
  (if (

(defmacro let (
|#

(defun fib (n)
  (if (< n 2)
      n
    (+ (fib (- n 2)) (fib (- n 1)))))

(list (fib 21)
      (fib 22))

a