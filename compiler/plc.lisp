(in-package :plc)

(defun compile-file (filepath output-filepath)
  (with-open-file (in filepath)
    (let ((*quote?* nil)
          (*bindings* '())
          (*local-var-index* 0))
      (let ((bytecodes (flatten (compile-impl (read in)))))
        (pvm:write-bytecodes-to-file output-filepath bytecodes))))
  t)



