(in-package :pc)

(defun compile (input)
  (let ((ast (etypecase input
               (string (parse-from-string input))
               (stream (parse input)))))
    (cp ast)))

(defun compile-to-file (input output-path)
  (with-open-file (out output-path :direction :output
                                   :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
    (write-sequence (compile input) out))
  t)

