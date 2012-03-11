(in-package :pvm)

(defstruct env
  stream
  stack
  rstack)

(defun read-uint (in)
  (loop FOR i FROM 3 DOWNTO 0 
        SUM (ash (read-byte in) (* i 8))))

(defun read-int (in)
  (let ((n (read-uint in)))
    (if (< n #x8FFFFFFF)
        n
      (- n #x100000000))))

(defun $read-int (env)
  (read-int (env-stream env)))

(defun $push (env x)
  (push x (env-stack env)))
