(in-package :pvm)

(defun execute (in)
  (pvme:execute-from-stream in))

(defun execute-from-file (path)
  (pvme:execute-from-file path))

(defun write-bytecodes-to-file (filepath codes)
  (pvm-bc::write-bc-to-file filepath codes))
