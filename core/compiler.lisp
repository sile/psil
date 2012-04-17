(in-package :pbc-core)

;; 簡易コンパイラ

(defun flatten (x)
  (cond ((null x) x)
        ((atom x) (list x))
        (t (append (flatten (car x)) (flatten (cdr x))))))

(defun int-to-bytes (n)
  (loop FOR i FROM 3 DOWNTO 0
        COLLECT (ldb (byte 8 (* i 8)) n)))

(defmacro @string (str)
  (let ((o (sb-ext:string-to-octets str)))
    `(list 2 ,@(int-to-bytes (length o)) ,@(coerce o 'list))))

;; (defmacro @symval (sym)
;;  (symbol-name sym)
;;(defun %call (name &rest args)

;;(defmacro @defun (name args body)
;;  `())
#|
(compile-bytecode 
 '(defun 
|#