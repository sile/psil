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

;; null
(defun null (list)
  (eq list nil))

;; mapcar 
(defun mapcar (fn list)
  (if (null list)
      nil
    (cons (fn (car list))
          (mapcar fn (cdr list)))))

;; first
(defun first (list)
  (car list))

;; second
(defun second (list)
  (car (cdr list)))

;; let
(defmacro let (binds &rest body)
  (cons (cons 'lambda (cons (mapcar first binds) body))
        (mapcar second binds)))

;; open/r, open/w
(defun open/r (path)
  (open path (+ o-rdonly)))

(defun open/w (path)
  (open path (+ o-creat o-rdwr)))

(mapcar (lambda (n) (+ n n 1)) '(1 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fib (n)
  (if (< n 2)
      n
    (+ (fib (- n 2)) (fib (- n 1)))))

(let ((n 10))
  (list (fib n) (fib (+ n 1))))
