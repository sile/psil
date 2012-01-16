;;;;;;;;;;;;;;
;;; parser ;;;
(defun @parse-file (path)
  (let ((in (open/r path)))
    (if (null in)
        nil
      (@parse in))))

(defun @parse (in)
  (read-byte in))

(@parse-file "data/fib.lisp")
