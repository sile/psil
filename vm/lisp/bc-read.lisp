(in-package :pvm)

(defun check-magic (in)
  (let ((magic (read-ascii-string in 4)))
    (assert (string= "psil" magic) () "Invalid Magic Code: ~a" magic)
    magic))

;;
(defun read-val-int (in)
  (read-int in))

(defun read-val-string (in)
  (let ((len (read-ushort in)))
    (utf8-octets-to-string (read-bytes in len))))

(defun read-val-char (in)
  (code-char (read-int in)))

(defun read-val-list (in)
  (let ((len (read-ushort in)))
    (loop REPEAT len COLLECT (read-constant in))))

(defun read-val-vector (in)
  (coerce (read-val-list in) 'vector))

#|
[encode format]
- tag:u1 value
- int:
  - tag=1, value=[i4]
- string:
  - tag=2, value=[len:i2] [utf8-str:i1*]
- char:
  - tag=3, value=[i4]
- vector:
  - tag=4, value=[len:i2] [value*]
- list:
  - tag=5, value=[len:i2] [value*]
|#
(defun read-constant (in)
  (let ((tag (read-octet in)))
    (ecase tag
      (1 #|int|# (read-val-int in))
      (2 #|string|# (read-val-string in))
      (3 #|char|# (read-val-char in))
      (4 #|vector|# (read-val-vector in))
      (5 #|list|# (read-val-list in))
      )))

(defun read-constant-table (in count)
  (loop REPEAT count COLLECT (read-constant in) INTO list
        FINALLY (return (coerce list 'vector))))

(defun read-symbol-table (in count constant-table)
  (declare (ignore in count constant-table))
  0)

(defun read-code-stream (in code-size)
  (declare (ignore in code-size))
  (make-byte-stream :bytes (make-array 0 :element-type 'octet)))

(defun collect-label-info (code-stream)
  (declare (ignore code-stream))
  0)

#|
[format]
- magic: "psil"
- symbol-count: u4
- constant-count: u4  ; constant => value
- code-size: u4
- symbol*
- constant*
- code*
|#
(defun read-bytecode (input-stream &aux (in input-stream))
  (check-magic in)
  (let ((version (read-uint in))
        (symbol-count (read-uint in))
        (constant-count (read-uint in))
        (code-size (read-uint in)))
    (let* ((constant-table (read-constant-table in constant-count))
           (symbol-table (read-symbol-table in symbol-count constant-table))
           (code-stream (read-code-stream in code-size))
           (label-table (collect-label-info code-stream)))
      (make-bcobj :id (gentemp)
                  :version version
                  :label-table label-table
                  :symbol-table symbol-table
                  :constant-table constant-table
                  :code-stream code-stream))))

(defun read-bytecode-from-file (file)
  (with-open-file (in file :element-type 'octet)
    (read-bytecode in)))

;; for development
(defun write-uint (out n)
  (loop FOR i FROM 3 DOWNTO 0
        DO (write-byte (ldb (byte 8 (* i 8)) n) out)))

(defun write-u (out n byte-width)
  (loop FOR i FROM (1- byte-width) DOWNTO 0
        DO (write-byte (ldb (byte 8 (* i 8)) n) out)))

(defun dev.write-constants (out)
  ;; int
  (write-u out 1 1)
  (write-uint out 123456)

  ;; string
  (write-u out 2 1)
  (let ((s "psil string"))
    (write-u out (length s) 2)
    (loop FOR o ACROSS (string-to-utf8-octets s) DO (write-u out o 1)))

  ;; vector
  (write-u out 4 1)
  (write-u out 3 2)
  (write-u out 1 1) (write-uint out 123)
  (write-u out 1 1) (write-uint out 456)
  (write-u out 3 1) (write-uint out (char-code #\7))
  )

(defun bc-write ()
  (with-open-file (out "/tmp/dev.bc" 
                       :direction :output
                       :element-type 'octet 
                       :if-exists :supersede)
    ;; magic
    (loop FOR c ACROSS "psil" DO (write-byte (char-code c) out))
    
    ;; version
    (write-uint out 1)

    ;; symbol-count
    (write-uint out 0)

    ;; constant-count
    (write-uint out 3)

    ;; code-size
    (write-uint out 0)
    
    ;; constant*
    (dev.write-constants out)

    ;; symbol*
    ;; code*
    ))
