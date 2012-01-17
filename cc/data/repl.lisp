
(defun write-string (s)
  (mapcar write-byte (string-to-list s))
  'ok)

(defun read (in)
  (@parse in))

(defun repl ()
  (write-string "> ")
  (let ((rlt (eval (read *stdin*))))
    (write-string " => ")
    (show rlt))
  (repl))

(repl)