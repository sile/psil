(in-package :pbc-core)

;; 簡易コンパイラ
;; => パッケージを分けた方が良さそう

(defun flatten (x)
  (cond ((null x) x)
        ((atom x) (list x))
        (t (append (flatten (car x)) (flatten (cdr x))))))

(defun $$ (x)
  (cond ((null x)   x)
        ((arrayp x) ($$ (coerce x 'list)))
        ((atom x)   (list x))
        (t (append  ($$ (car x)) ($$ (cdr x))))))

(defun $ (&rest x)
  (apply #'append (mapcar #'$$ x)))

(defun int-to-bytes (n)
  (loop FOR i FROM 3 DOWNTO 0
        COLLECT (ldb (byte 8 (* i 8)) n)))

(defun short-to-bytes (n)
  (loop FOR i FROM 1 DOWNTO 0
        COLLECT (ldb (byte 8 (* i 8)) n)))

(defun @int (n)
  (declare ((signed-byte 32) n))
  ($ 1 (int-to-bytes n)))

(defun @string (str)
  (declare (simple-string str))
  (let ((o (sb-ext:string-to-octets str)))
    ($ 2 (int-to-bytes (length o)) o)))

(defun @char (ch)
  (declare (character ch))
  ($ 3 (int-to-bytes (char-code ch))))

(defun @symbol (symbol)
  (declare (symbol symbol))
  (let* ((name (symbol-name symbol))
         (octets (sb-ext:string-to-octets name)))
    ($ 4 (short-to-bytes (length octets)) octets)))

(defun @nil () ($ 5))
(defun @true () ($ 6))
(defun @false () ($ 7))

;; (defmacro @symval (sym)
;;  (symbol-name sym)
;;(defun %call (name &rest args)

;;(defmacro @defun (name args body)
;;  `())
#|
(compile-bytecode 
 '(defun 
|#