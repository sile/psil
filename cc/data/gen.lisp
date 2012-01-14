(defun write-lint (n byte-width out)
  (loop FOR i FROM 0 BELOW byte-width
        DO
        (write-byte (ldb (byte 8 (* i 8)) n) out)))

(defparameter *symbols*
  '(
    (0 "NIL") ; reserved
    (1 "T")
    (2 "HELLO")
    ))

(defparameter *data*
  '(
    (0 (:symbol 0))
    (1 (:symbol 1))
    (2 (:string "Hello World!"))
    ))

(defun write-header (out)
  ;; version
  (write-lint 1 4 out)

  ;; symbol-count 
  (write-lint (length *symbols*) 4 out)

  ;; init-data-count
  (write-lint (length *data*) 4 out)
  )

(defun write-psil-string (str out)
  (let ((octets (string-to-octets str)))
    (write-lint (length octets) 4 out)
    (write-sequence octets out)))

(defun write-symbol-table (out)
  (loop FOR (code name) IN *symbols*
        DO
        (write-lint code 4 out)
        (write-psil-string name out)))

(defun type-code (type)
  (position type '(:object :cons :list :string :refer :integer
                           :symbol :quote)))

(defun write-symbol (fields out)
  (assert (= (length fields) 1))
  (let ((symbol-code (first fields)))
    (write-lint symbol-code 4 out)))

(defun @write-string (fields out)
  (assert (= (length fields) 1))
  (let ((str (first fields)))
    (write-psil-string str out)))

(defun @write-quote (fields out)
  (assert (= (length fields) 1))
  (let ((x (first fields)))
    (write-data x out)))

(defun write-data (data out)
  (destructuring-bind (type . fields) data
    (write-lint (type-code type) 4 out)
    (ecase type
      (:symbol (write-symbol fields out))
      (:string (@write-string fields out))
      (:quote (@write-quote fields out))
      )))

(defun write-init-data (out)
  (loop FOR (sym-code data) IN *data*
        DO
        (write-lint sym-code 4 out)
        (write-data data out)))

#+C
(defparameter *body* 
  '(:string "abc"))

(defparameter *body* 
  '(:quote (:symbol 2)))

#+C
(defparameter *body* 
  '(:symbol 2))

(defun write-body (out)
  (write-data *body* out))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (out "core.bin" :direction :output
                     :if-exists :supersede 
                     :element-type 'octet)
  ;; header
  (write-header out)
  
  ;; symbol-table
  (write-symbol-table out)

  ;; init-data
  (write-init-data out)

  ;; body
  (write-body out)
  )