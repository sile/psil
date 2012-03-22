(in-package :pvm)

(defun read-ascii-string (in n)
  (map 'string #'code-char (loop REPEAT n COLLECT (read-byte in))))

(defun read-unsigned-integer (in byte-width)
  (loop FOR i FROM (1- byte-width) DOWNTO 0
        SUM (ash (read-byte in) (* i 8))))

(defun read-integer (in byte-width)
  (let* ((limit (ash 1 (* byte-width 8)))
         (positive-limit (ash limit -1))
         (n (read-unsigned-integer in byte-width)))
    (if (< n positive-limit)
        n
      (- n limit))))

(defun read-octet (in) (read-unsigned-integer in 1))

(defun read-short (in) (read-integer in 2))
(defun read-int (in) (read-integer in 4))
(defun read-long (in) (read-integer in 8))

(defun read-ushort (in) (read-unsigned-integer in 2))
(defun read-uint (in) (read-unsigned-integer in 4))
(defun read-ulong (in) (read-unsigned-integer in 8))

(defun read-bytes (in length)
  (let ((buf (make-array length :element-type 'octet)))
    (read-sequence buf in)
    buf))

(defun utf8-octets-to-string (octets)
  (sb-ext:octets-to-string octets :external-format :utf8))

(defun string-to-utf8-octets (str)
  (sb-ext:string-to-octets str :external-format :utf8))

  

        