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

(defun read-ushort (in)
  (loop FOR i FROM 1 DOWNTO 0 
        SUM (ash (read-byte in) (* i 8))))

(defun read-short (in)
  (let ((n (read-ushort in)))
    (if (< n #x8FFF)
        n
      (- n #x10000))))

(defun $eos (env)
  (not (listen (env-stream env))))

(defun $read-op (env)
  (read-ushort (env-stream env)))

(defun $read-int (env)
  (read-int (env-stream env)))
(define-symbol-macro @read-int ($read-int env))

(defun $push (env x)
  (declare (int4 x))
  (push x (env-stack env)))
(defmacro @push (x) `($push env ,x))

(defun $pop (env)
  (the int4 (pop (env-stack env))))
(define-symbol-macro @pop ($pop env))

(defun $head (env)
  (the int4 (first (env-stack env))))
(define-symbol-macro @head ($head env))

(defun $swap (env &aux (stack (env-stack env)))
  (rotatef (first stack) (second stack)))
(define-symbol-macro @swap ($swap env))

(defun $ref (env i)
  (nth i (env-stack env)))
(defmacro @ref (i) `($ref env ,i))

(defun $rot (env &aux (stack (env-stack env)))
  (rotatef (first stack) (third stack) (second stack)))

(defun empty-env (stream)
  (make-env :stream stream))
