(in-package :pvm-bc)

(defun read-int (in)
  (let ((n (loop FOR i FROM 3 DOWNTO 0 
                 SUM (ash (read-byte in nil nil) (* i 8)))))
    (if (< n #x80000000)
        n
      (- n #x100000000))))
