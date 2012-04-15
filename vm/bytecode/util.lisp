(in-package :pvm-bc)

(defun read-int (in)
  (let ((n (loop FOR i FROM 3 DOWNTO 0 
                 SUM (ash (read-byte in nil nil) (* i 8)))))
    (if (< n #x80000000)
        n
      (- n #x100000000))))

(defun write-string-as-octets (out str)
  (declare (simple-string str)) 
  (write-sequence (sb-ext:string-to-octets str) out)
  t)

(defun write-int (out n)
  (loop FOR i FROM 3 DOWNTO 0
        DO (write-byte (ldb (byte 8 (* i 8)) n) out))
  t)
