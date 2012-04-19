(in-package :plc)

(defun compile-file (filepath output-filepath)
  (with-open-file (in filepath)
    (with-env ()
      (let ((bytecodes (flatten (compile-impl (read in)))))
        (pvm:write-bytecodes-to-file output-filepath bytecodes))))
  t)
