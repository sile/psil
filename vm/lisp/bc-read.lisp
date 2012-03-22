(in-package :pvm)

(defun check-magic (in)
  (let ((magic (read-ascii-string in 4)))
    (assert (string= "psil" magic) () "Invalid Magic Code: ~a" magic)
    magic))

(defun read-symbol-table (in count)
  (declare (ignore in count))
  0)

(defun read-constant-table (in count)
  (declare (ignore in count))
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
- constant-count: u4
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
    (let* ((symbol-table (read-symbol-table in symbol-count))
           (constant-table (read-constant-table in constant-count))
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
    (write-uint out 0)

    ;; code-size
    (write-uint out 0)

    ;; code
    ))
