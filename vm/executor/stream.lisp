(in-package :pvme)

(defstruct octets-stream
  (pos    0 :type fixnum)
  (octets t :type octets))

(defun make-code-stream (codes)
  (make-octets-stream :octets codes))

(defmacro read-unsigned (stream byte-width)
  `(with-slots (pos octets) (the octets-stream ,stream)
     (loop FOR i FROM ,(1- byte-width) DOWNTO 0
           SUM (prog1 (ash (aref octets pos) (* 8 i))
                 (incf pos)))))

(defun read-ubyte (stream)
  (read-unsigned stream 1))

(defun read-ushort (stream)
  (read-unsigned stream 2))

(defun read-uint (stream)
  (read-unsigned stream 4))
(defun read-int (stream)
  (let ((n (read-uint stream)))
    (if (< n #x80000000)
        n
      (- n #x100000000))))

(defun read-ulong (stream)
  (read-unsigned stream 8))

(defun read-op (stream)
  (read-ubyte stream))

(defun read-string (stream length)
  (with-slots (pos octets) stream
    (prog1 (sb-ext:octets-to-string (subseq octets pos (+ pos length)))
      (incf pos length))))

(defun eos? (stream)
  (with-slots (pos octets) (the octets-stream stream)
    (>= pos (length octets))))

(defun get-pc (stream)
  (octets-stream-pos stream))

(defun set-pc (stream pc)
  (setf (octets-stream-pos stream) pc)
  stream)
