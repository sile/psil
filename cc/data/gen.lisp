(defun write-lint (n byte-width out)
  (loop FOR i FROM 0 BELOW byte-width
        DO
        (write-byte (ldb (byte 8 (* i 8)) n) out)))

(defparameter *symbols*
  '(
    (0 "NIL") ; reserved
    (1 "T")
    (2 "HELLO")
    (3 "LAMBDA")
    (4 "PROGN")
    (5 "A")
    (6 "B")
    (7 "C")
    (8 "IF")
    (9 "LAMDBA-MACRO")
    ))

(defparameter *data*
  '(
    (0 (:symbol 0))
    (1 (:symbol 1))
    (2 (:string "Hello World!"))
    (3 (:special 0)) ; 0 = lambda
    (4 (:special 1)) ; 1 = progn
    (8 (:special 2)) ; 2 = if
    (9 (:special 3)) ; 3 = lambda-macro
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
                           :symbol :quote :function :special)))

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

(defun @write-integer (fields out)
  (assert (= (length fields) 1))
  (let ((x (first fields)))
    (write-lint x 4 out)))

(defun @write-list (fields out)
  (assert (= (length fields) 1))
  (write-lint (length (first fields)) 4 out)
  (loop FOR x IN (first fields)
        DO
        (write-data x out)))

(defun @write-special (fields out)
  (@write-integer fields out))
  
(defun write-data (data out)
  (destructuring-bind (type . fields) data
    (write-lint (type-code type) 4 out)
    (ecase type
      (:symbol (write-symbol fields out))
      (:string (@write-string fields out))
      (:quote (@write-quote fields out))
      (:integer (@write-integer fields out))
      (:list (@write-list fields out))
      (:special (@write-special fields out))
      )))

(defun write-init-data (out)
  (loop FOR (sym-code data) IN *data*
        DO
        (write-lint sym-code 4 out)
        (write-data data out)))

#+C
(defparameter *body* 
  '(:string "abc"))

#+C
(defparameter *body* 
  '(:list ((:integer 1) (:integer 2) (:integer 3))))

#+C
(defparameter *body*
  '(:integer 10))

#+C
(defparameter *body* 
  '(:symbol 3))

#+C
(defparameter *body* 
  '(:list ((:symbol 3)  ; lambda
           (:list ())   ; ()
           (:integer 1)
           (:integer 2)
           )))
#+C
(defparameter *body* 
  '(:list 
    ((:list ((:symbol 3)  ; lambda
             (:list ((:symbol 5)))   ; (A)
             (:integer 1)
             (:symbol 2)
             (:symbol 5)))
     (:integer 10))))

#+C
(defparameter *body* 
  '(:list ((:symbol 4)  ; (progn 1 2)
           (:integer 1)
           (:integer 2))))

#+C
(defparameter *body*
  '(:list ((:special 2) ; if
           (:symbol 0)  ; nil
           (:integer 2)
           (:integer 3))))

(defparameter *body* 
  '(:list
    ((:list ((:symbol 9)  ; lambda-macro
           (:list ())   ; ()
           (:integer 1)
           (:integer 2)
           )))
    ))

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