(defun write-lint (n byte-width out)
  (loop FOR i FROM 0 BELOW byte-width
        DO
        (write-byte (ldb (byte 8 (* i 8)) n) out)))

(defparameter *symbols*
  '(
    (0 "NIL") ; reserved
    (1 "T") ; reserved
    (2 "HELLO")
    (3 "LAMBDA")
    (4 "PROGN")
    (5 "A")
;    (6 "B")
;    (7 "C")
    (8 "IF")
    (9 "LAMBDA-MACRO")
    
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
    
    (30 "*STDIN*")
    (31 "*STDOUT*")
    (32 "*STDERR*")

    (33 "FIB")
    (34 "N")

    (35 "LIST")
    (36 "DEFUN")
    (37 "FN-NAME")
    (38 "ARGS")
    (39 "BODY")
    (40 "QUOTE")

    (41 "O-CREAT")
    (42 "O-EXCL")
    (43 "O-RDONLY")
    (44 "O-WRONLY")
    (45 "O-RDWR")
    ))

(defparameter *data*
  '(
    ((:symbol "NIL") (:symbol "NIL"))
    ((:symbol "T") (:symbol "T"))
    ((:symbol "LAMBDA") (:special 0)) ; 0 = lambda
    ((:symbol "PROGN") (:special 1)) ; 1 = progn
    ((:symbol "IF") (:special 2)) ; 2 = if
    ((:symbol "LAMBDA-MACRO") (:special 3)) ; 3 = lambda-macro

    ((:symbol "+") (:native-function 0)) ; +
    ((:symbol "-") (:native-function 1)) ; -
    ((:symbol "*") (:native-function 2)) ; *
    ((:symbol "/") (:native-function 3)) ; /
    ((:symbol "=") (:native-function 4)) ; =
    ((:symbol "<") (:native-function 5)) ; <

    ((:symbol "CAR") (:native-function 6)) ; car
    ((:symbol "CDR") (:native-function 7)) ; cdr
    ((:symbol "CONS") (:native-function 8)) ; cons
    
    ((:symbol "EQ") (:native-function 9)) ; eq
    ((:symbol "TYPE-OF") (:native-function 10)) ; type-of
    ((:symbol "SET-SYMBOL-VALUE") (:native-function 11)) ; set-symbol-value
    ((:symbol "SET-CAR") (:native-function 12)) ; set-car
    ((:symbol "SET-CDR") (:native-function 13)) ; set-cdr
    
    ((:symbol "OPEN") (:native-function 14)) ; open
    ((:symbol "CLOSE") (:native-function 15)) ; close
    ((:symbol "READ-BYTE") (:native-function 16)) ; read-byte
    ((:symbol "WRITE-BYTE") (:native-function 17)) ; write-byte
    
    ((:symbol "INTERN") (:native-function 18)) ; intern
    ((:symbol "INTERN2") (:native-function 19)) ; symbol-value

    ((:symbol "*STDIN*") (:stream 0)) ; FD: 0
    ((:symbol "*STDOUT*") (:stream 1)) ; FD: 1
    ((:symbol "*STDERR*") (:stream 2)) ; FD: 2

    ((:symbol "LIST") (:native-function 20)) ; list
    ((:symbol "LIST-TO-STRING") (:native-function 21)) ; list-to-string
    ((:symbol "STRING-TO-LIST") (:native-function 22)) ; string-to-list
    ((:symbol "SHOW") (:native-function 23)) ; show
    ((:symbol "MOD") (:native-function 24))

    ((:symbol "QUOTE") (:special 4)) ; quote
    ((:symbol "SYMBOL-MACRO") (:special 5)) ; symbol-macro
    
    ((:symbol "O-CREAT") (:integer 64)) ; O_CREAT
    ((:symbol "O-EXCL") (:integer 128)) ; O_EXCL
    ((:symbol "O-RDONLY") (:integer 0)) ; O_RDONLY
    ((:symbol "O-WRONLY") (:integer 1)) ; O_WRONLY
    ((:symbol "O-RDWR") (:integer 2)) ; O_RDWR
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
                           :macro-function :native-function 
                           :stream :symbol-macro)))

(defun write-symbol (fields out)
  (assert (= (length fields) 1))
  (let ((symbol-name (first fields)))
    (write-psil-string symbol-name out)))

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

(defun @write-stream (fields out)
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
      (:stream (@write-stream fields out))
      (:list (@write-list fields out))
      (:special (@write-special fields out))
      (:native-function (@write-native-function fields out))
      )))

(defun write-init-data (out)
  (loop FOR (sym data) IN *data*
        DO
        (write-data sym out)
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

(defparameter *body* 
  '(:symbol "NIL"))

#+C
(defparameter *body* 
  '(:list ((:symbol "LAMBDA")  ; lambda
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

#+C
(defparameter *body* 
  '(:list ((:symbol 15) ; <
           (:integer 98)
           (:integer 99)
           (:integer 100)
           )))
#+C
(defparameter *body* 
  '(:list ((:symbol 18)  ; cons
           (:symbol 0)
           (:integer 2))))

#+C
(defparameter *body* 
  '(:list ((:symbol 27)  ; write-byte
           (:integer 90)
           (:symbol 31))))

#+C
(defparameter *body* 
  '(:list ((:symbol 4) ; progn
           (:list ((:symbol 21)  ; (set-symbol-value a 40)
                   (:quote (:symbol 5))
                   (:integer 40)))
           (:list ((:symbol 10) ; (+ a 2 a)
                   (:symbol 5)
                   (:integer 2)
                   (:symbol 5))))))

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