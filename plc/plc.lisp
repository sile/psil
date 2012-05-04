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

(defun compile (exp)
  (plcc:compile exp))

(defun compile-file (in-file out-file)
  (plcc:compile-file in-file out-file))

(defun compile-string (str out-file)
  (plcc::compile-string str out-file))
