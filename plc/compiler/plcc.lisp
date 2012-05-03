(in-package :plcc)

(defun compile (sexp)
  (declare (ignore sexp))
  :todo)

(defun output-to-file (compiled out-file)
  (declare (ignore compiled out-file))
  :todo)

(defun compile-file (in-file out-file)
  (let* ((sexp (with-open-file (in in-file) (plcp:parse in)))
         (compiled (compile sexp)))
    (output-to-file compiled out-file)))
