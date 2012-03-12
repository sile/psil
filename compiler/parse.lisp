(in-package :pc)

(defun parse-impl (in)
  (read in)) ; XXX: 簡易的

(defun parse (in)
  (parse-impl in))

(defun parse-from-string (str)
  (with-input-from-string (in str)
    (parse in)))
