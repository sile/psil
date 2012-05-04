(in-package :plcc)

(defun compile (exp)
  (compile-impl exp))

(defun output-to-file (bcobj out-file)
  (with-open-file (out out-file 
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (output-to-stream out bcobj)))

(defun compile-file (in-file out-file)
  (let* ((sexp (with-open-file (in in-file) (plcp:parse in)))
         (compiled (compile sexp)))
    (output-to-file compiled out-file)))
