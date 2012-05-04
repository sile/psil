(in-package :plcc)

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

(defun write-int (out n)
  (loop FOR i FROM 3 DOWNTO 0
        DO (write-byte (ldb (byte 8 (* 8 i)) n) out)))
