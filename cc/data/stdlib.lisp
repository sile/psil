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
