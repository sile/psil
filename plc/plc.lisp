(in-package :plc)

(defun parse (in)
  (values (plcp:parse in)
          (file-position in)))

(defun parse-file (filepath)
  (with-open-file (in filepath)
    (parse in)))

(defun parse-string (str)
  (with-input-from-string (in str)
    (parse in)))
