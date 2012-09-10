(in-package :plcp)

(defun peek-ch (in)
  (peek-char nil in nil #\Null))

(defun read-ch (in)
  (read-char in nil #\Null))
