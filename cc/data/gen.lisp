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
    
    (10 "+")
    (11 "-")
    (12 "*")
    (13 "/")
    (14 "=")
    (15 "<")

    (16 "CAR")
    (17 "CDR")
    (18 "CONS")
    
    (19 "EQ")
    (20 "TYPE-OF")
    (21 "SET-SYMBOL-VALUE")
    (22 "SET-CAR")
    (23 "SET-CDR")
    
    (24 "OPEN")
    (25 "CLOSE")
    (26 "READ-BYTE")
    (27 "WRITE-BYTE")
    
    (28 "INTERN")
    (29 "SYMBOL-VALUE")
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

    (10 (:native-function 0)) ; +
    (11 (:native-function 1)) ; -
    (12 (:native-function 2)) ; *
    (13 (:native-function 3)) ; /
    (14 (:native-function 4)) ; =
    (15 (:native-function 5)) ; <

    (16 (:native-function 6)) ; car
    (17 (:native-function 7)) ; cdr
    (18 (:native-function 8)) ; cons
    
    (19 (:native-function 9)) ; eq
    (20 (:native-function 10)) ; type-of
    (21 (:native-function 11)) ; set-symbol-value
    (22 (:native-function 12)) ; set-car
    (23 (:native-function 13)) ; set-cdr
    
    (24 (:native-function 14)) ; open
    (25 (:native-function 15)) ; close
    (26 (:native-function 16)) ; read-byte
    (27 (:native-function 17)) ; write-byte
    
    (28 (:native-function 18)) ; intern
    (29 (:native-function 19)) ; symbol-value
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
                           :symbol :quote :function :special
                           :macro-function :native-function)))

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

(defun @write-native-function (fields out)
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
      (:native-function (@write-native-function fields out))
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

#+C
(defparameter *body* 
  '(:list
    ((:list ((:symbol 9)  ; lambda-macro
           (:list ())   ; ()
           (:integer 1)
           (:integer 2)
           )))
    ))

#+C
(defparameter *body* 
  '(:list ((:symbol 13) ; /
           (:integer 100)
           (:integer 3)
           (:integer 10)
           )))

(defparameter *body* 
  '(:list ((:symbol 15) ; <
           (:integer 98)
           (:integer 99)
           (:integer 100)
           )))

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