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
#|
((lambda (x)
   (+ x x))
 10)
|#

#+C
(defmacro macro-let (binds &rest body)
  (cons (list 'lambda-macro (mapcar first binds) (cons 'progn body))
        (mapcar second binds)))

(defmacro macro-let (binds &rest body)
  (show
  (append (list 'let (mapcar (lambda (bind)
                               (list (first bind)
                                     (list 'lambda-macro nil (second bind))))
                             binds))
          body)))

(defmacro labels (binds &rest body)
  (let ((main (cons (cons 'lambda (cons (mapcar (lambda (c) 'a) #+C first binds) body))
                    (mapcar (lambda (bind)
                              (list 'lambda (mapcar (lambda (c) 'a) #+C first binds) (second bind)))
                            binds))))
    
    (list 'macro-let (mapcar (lambda (bind)
                               (list (first bind)
                                     (cons 'a #+C(first bind) (mapcar (lambda (c) 'a) #+C first binds))))
                             binds)
          main)))
#|
((lambda (fib)
   ((fib fib) 10))
 (lambda (fib)
   (lambda (x)
     (if (< x 2)
         x
       (+ ((fib fib) (1- x)) 2)))))
|#

;; open/r, open/w
(defun open/r (path)
  (open path (+ o-rdonly)))

(defun open/w (path)
  (open path (+ o-creat o-rdwr)))

;; append
(defun append (list1 list2)
  (if (null list1)
      list2
    (cons (car list1) (append (cdr list1) list2))))

;; string-upcase
(defun string-upcase (str)
  (list-to-string
   (mapcar (lambda (c)
             (if (< 96 c 123) ; a-z
                 (- c 32)
               c))
           (string-to-list str))))

;; every
(defun every (fn list)
  (if (null list)
      t
    (if (fn (car list))
        (every fn (cdr list))
      nil)))

;; parse-integer
(defun parse-integer (str)
  (setq recur (lambda (n bytes)
                (if (null bytes)
                    n
                  (recur (+ (* n 10) (car bytes)) (cdr bytes)))))
  (recur 0 (mapcar (lambda (c) (- c 48)) (string-to-list str))))
