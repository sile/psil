(in-package :pvm)

(defstruct env
  stream
  stack
  rstack
  (heap (make-hash-table))
  (heap-index 0 :type fixnum)
  (vars (make-hash-table))
  (label=>addr (make-hash-table)))

(defun empty-env (stream)
  (make-env :stream stream))

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

(defun $r.push (env x)
  (declare (int4 x))
  (push x (env-rstack env)))
(defmacro @r.push (x) `($r.push env ,x))

(defun $r.pop (env)
  (the int4 (pop (env-rstack env))))
(define-symbol-macro @r.pop ($r.pop env))

(defun $r.head (env)
  (first (env-rstack env)))
(define-symbol-macro @r.head ($r.head env))

(defun $r.ref (env i)
  (nth i (env-rstack env)))
(defmacro @r.ref (i) `($r.ref env ,i))

(defun $r.set (env i val)
  (setf (nth i (env-rstack env)) val))
(defmacro @r.set (i val) `($r.set env ,i ,val))

(defun $h.register (env x)
  (with-slots (heap heap-index) env
    (setf (gethash (incf heap-index) heap) x)
    heap-index))

(defun $h.deregister (env index)
  (unless (gethash index (env-heap env))
    (error "wrong heap index: ~a" index))
  (remhash index (env-heap env)))

(defun $m.ref (env h.index m.index)
  (aref (gethash h.index (env-heap env)) m.index))

(defun $m.set (env h.index m.index value)
  (setf (aref (gethash h.index (env-heap env)) m.index) value))

(defun $pc (env)
  (file-position (env-stream env)))
(define-symbol-macro @pc ($pc env))

(defun $set-pc (env pos)
  (file-position (env-stream env) pos))

(defun $jump (env pos)
  ($set-pc env pos))

(defun $add-label (env)
  (setf (gethash @read-int (env-label=>addr env)) @pc))

(defun $label-addr (env label)
  (assert #1=(gethash label (env-label=>addr env)) () "unknown label: ~a" label)
  #1#)

(defun $v.register (env var-index)
  (setf (gethash var-index (env-vars env)) 0))

(defun $v.get (env var-index)
  (assert #1=(gethash var-index (env-vars env)) () "unknown variable: ~a" var-index)
  #1#)

(defun $v.set (env var-index value)
  (assert #1=(gethash var-index (env-vars env)) () "unknown variable: ~a" var-index)
  (setf #1# value))
