(in-package :plc)

;; 簡易版
(defun flatten (x)
  (cond ((null x) x)
        ((atom x) (list x))
        (t (append (flatten (car x)) (flatten (cdr x))))))

(defun int-to-bytes (n)
  (loop FOR i FROM 3 DOWNTO 0
        COLLECT (ldb (byte 8 (* i 8)) n)))

(defun short-to-bytes (n)
  (loop FOR i FROM 1 DOWNTO 0
        COLLECT (ldb (byte 8 (* i 8)) n)))