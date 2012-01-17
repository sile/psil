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

(defun mapcar2 (fn list1 list2)
  (if (or (null list1) (null list2))
      nil
    (cons (fn (car list1) (car list2))
          (mapcar2 fn (cdr list1) (cdr list2)))))

;; or
(defmacro or (&rest exps)
  (if (null exps)
      nil
    (let ((v (gensym.tmp)))
      (list 'let (list (list v (car exps)))
        (list 'if v v (cons 'or (cdr exps)))))))

;; and
(defmacro and (&rest exps)
  (if (null exps)
      t
    (let ((v (gensym.tmp)))
      (list 'let (list (list v (car exps)))
        (list 'if v (cons 'and (cdr exps)) v)))))

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

;; => symbol-macrolet
(defmacro macro-let (binds &rest body)
  (append (list 'let (mapcar (lambda (bind)
                               (list (first bind)
                                     (list 'symbol-macro (second bind))))
                             binds))
          body))

(defun reverse-impl (list acc)
  (if (null list)
      acc
    (reverse-impl (cdr list) (cons (car list) acc))))

(defun reverse (list)
  (reverse-impl list nil))
    
(defun integer-to-string-impl (n)
  (let ((i (+ (mod n 10) 48)))
    (if (< 0 (/ n 10))
        (cons i (integer-to-string-impl (/ n 10)))
      (list i))))

(defun integer-to-string (n)
  (list-to-string (reverse (integer-to-string-impl n))))

(defun string+ (s1 s2)
  (list-to-string
   (append (string-to-list s1) (string-to-list s2))))
           
(setq *gensym.seq* 0)
(defun gensym.tmp ()
  (let ((sym (intern (string+ "GEN_" (integer-to-string *gensym.seq*)))))
    (setq *gensym.seq* (+ *gensym.seq* 1))
    sym))
  
(defun length (list)
  (if (null list)
      0
    (+ 1 (length (cdr list)))))

(defun gensym-list (n)
  (if (= n 0)
      '()
    (cons (gensym.tmp)
          (gensym-list (- n 1)))))

(defmacro let-rec (binds &rest body)
  (let ((args (gensym-list (length binds))))
    (let ((main (cons (cons 'lambda (cons args body))
                      (mapcar (lambda (bind)
                                (list 'lambda args (second bind)))
                              binds))))
    
      (list 'macro-let (mapcar2 (lambda (bind arg)
                                  (list (first bind)
                                        (cons arg args)))
                                binds args)
          main))))
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
  (let-rec ((recur (lambda (n bytes)
                     (if (null bytes)
                         n
                       (recur (+ (* n 10) (car bytes)) (cdr bytes))))))
    (recur 0 (mapcar (lambda (c) (- c 48)) (string-to-list str)))))
