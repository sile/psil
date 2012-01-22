(in-package :psil)

(defun execute-bytecode (bytecode-stream)
  (psil.bytecode-executor:execute bytecode-stream))

(defun execute-bytecode-from-file (filepath)
  (with-open-file (in filepath :element-type 'octet)
    (execute-bytecode in)))

(defun execute-bytecode-from-octets (octets)
  (with-open-file (out "/tmp/psil.tmp"
                       :direction :output
                       :element-type 'octet
                       :if-exists :supersede)
    (loop FOR o ACROSS octets
          DO (write-byte o out)))
  (execute-bytecode-from-file "/tmp/psil.tmp"))

(defun exec (in)
  (etypecase in
    (stream (execute-bytecode in))
    (string (execute-bytecode-from-file in))
    (array  (execute-bytecode-from-octets in))))
