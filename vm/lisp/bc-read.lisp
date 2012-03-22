(in-package :pvm)

(defun check-magic (in)
  (let ((magic (read-ascii-string in 4)))
    (assert (string= "psil" magic) () "Invalid Magic Code: ~a" magic)
    magic))

(defun read-label-table (in count)
  0)

(defun read-symbol-table (in count)
  0)

(defun read-constant-table (in count)
  0)

(defun read-code-stream (in)
  (make-byte-stream))

(defun read-bytecode (input-stream &aux (in input-stream))
  (check-magic in)
  (let ((label-count (read-uint in))
        (symbol-count (read-uint in))
        (constant-count (read-uint in)))
    (make-bcobj :id (gentemp)
                :label-table (read-label-table in label-count)
                :symbol-table (read-symbol-table in symbol-count)
                :constant-table (read-constant-table in constant-count)
                :code-stream (read-code-stream in))))

(defun read-bytecode-from-file (file)
  (with-open-file (in file :element-type 'octet)
    (read-bytecode in)))
