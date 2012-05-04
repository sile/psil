(in-package :plcc)

(defstruct bcobj 
  constants
  code)

(defparameter *version* 1)
(defparameter *magic* (sb-ext:string-to-octets "psil"))

(defun compile-constants (constants)
  (flatten (map 'list (lambda (c)
                        (declare (symbol c))
                        (let ((name (sb-ext:string-to-octets (symbol-name c))))
                          (list (short-to-bytes (length name)) (coerce name 'list))))
                constants)))

(defun output-header (out constant-count constants-size code-size)
  (write-sequence *magic* out)  ; magic
  (write-int out *version*)     ; version
  
  (let ((constant-start (+ 4 4 4 4 4 4)))
    (write-int out constant-start) ; constant-start
    (write-int out constant-count) ; constant-count
    (write-int out (+ constant-start constants-size)) ; code-start
    (write-int out code-size))
  t)

(defun output-to-stream (out bcobj)
  (with-slots (constants code) bcobj
    (let ((constant-bc (compile-constants constants)))
      (output-header out (length constants) (length constant-bc) (length code))
      (write-sequence constant-bc out)
      (write-sequence code out)))
  (force-output out)
  t)
