(in-package :psil)

(defun read (in)
  (psil.reader:read in))

(defun read-from-string (str)
  (with-input-from-string (in str)
    (read in)))
