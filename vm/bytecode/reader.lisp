(in-package :pvm-bc)

#|
[format]
- header
 - magic: char[4]#"psil"
 - version: int
 - symbol-count: int
 - code_size: int
 - symbol*: string
- code
|#

(defstruct header
  (version      0 :type fixnum)
  (symbol-count 0 :type fixnum)
  (code-size    0 :type fixnum))

(defun header (&key (version 1) code-size (symbol-count 0))
  (make-header :version version 
               :symbol-count symbol-count  ; XXX: => constant-table/constant-count
               :code-size code-size))

(defstruct bc
  (header       t :type header)
  (symbol-table t :type simple-vector) 
  (codes        t :type octets))

(defmethod print-object ((o bc) stream)
  (print-unreadable-object (o stream :type t :identity t)))

(defparameter *magic-string* "psil")
(defun check-magic (in)
  (assert (every (lambda (c) (= (char-code c) (read-byte in nil nil))) *magic-string*)
          () "malformed magic string")
  t)

(defun read-header (in)
  (check-magic in)
  (make-header :version (read-int in)
               :symbol-count (read-int in)
               :code-size (read-int in)))

(defun read-symbol-table (in count)
  (loop REPEAT count
        COLLECT (let* ((ins (read-byte in)) ; discard
                       (len (read-short in))
                       (oct (read-octets in len))
                       (sym (intern (sb-ext:octets-to-string oct) :keyword)))
                  (declare (ignore ins))
                  sym)
        INTO list
        FINALLY
        (return (coerce list 'vector))))
                  
(defun read-from-stream (in)
  (let ((header (read-header in)))
    (make-bc :header header
             :symbol-table (read-symbol-table in (header-symbol-count header))
             :codes (read-octets in (header-code-size header)))))

(defun read-from-file (path)
  (with-open-file (in path :element-type 'octet)
    (read-from-stream in)))
