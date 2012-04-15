(in-package :pvm-bc)

#|
[format]
- header
 - magic: char[4]#"psil"
 - version: int
 - code_size: int
- code
|#

(defstruct header
  (version   0 :type fixnum)
  (code-size 0 :type fixnum))

(defun header (&key (version 1) code-size)
  (make-header :version version 
               :code-size code-size))

(defstruct bc
  (header t :type header)
  (codes  t :type octets))

(defparameter *magic-string* "psil")
(defun check-magic (in)
  (assert (every (lambda (c) (= (char-code c) (read-byte in nil nil))) *magic-string*)
          () "malformed magic string")
  t)

(defun read-header (in)
  (check-magic in)
  (make-header :version (read-int in)
               :code-size (read-int in)))

(defun read-from-stream (in)
  (let ((header (read-header in)))
    header))

(defun read-from-file (path)
  (with-open-file (in path :element-type 'octet)
    (read-from-stream in)))
