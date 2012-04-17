(in-package :pvm)

(defun execute (in)
  (pvme:execute-from-stream in))

(defun execute-from-file (path)
  (pvme:execute-from-file path))
