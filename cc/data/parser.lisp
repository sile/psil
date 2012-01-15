
(defun @read (path)
  (with-open-file (in path)
    (read in)))

(defun init-symbols (&aux (m (make-hash-table)))
  (loop FOR (code name) IN *symbols*
        DO
        (setf (gethash (intern name) m) code))
  m)

(defun @compile (exp)
  (if (eq exp t)
      `(:symbol 1)
    (etypecase exp
      (fixnum `(:integer ,exp))
      (string `(:string ,exp))
      (null `(:symbol "NIL"))
      (symbol `(:symbol ,(symbol-name exp)))
      (cons (if (eq (car exp) 'quote)
                `(:quote ,(@compile (second exp)))
              `(:list ,(mapcar (lambda (e)
                                 (@compile e))
                               exp)))))))

(define-symbol-macro d
 (with-open-file (out "fib.bin" :direction :output
                     :if-exists :supersede 
                     :element-type 'octet)
  (let ((*symbols* '())
        (*data* '())
        (*body* (@compile (@read "fib.lisp"))))
    ;; header
    (write-header out)

    ;; symbol-table
    (write-symbol-table out)
    
    ;; init-data
    (write-init-data out)
    
    ;; body
    (write-body out))))

(define-symbol-macro e (@compile (@read "fib.lisp")))
