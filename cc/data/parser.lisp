
(defun @read (path)
  (with-open-file (in path)
    (read in)))

(defun @read2 (path)
  (with-open-file (in path)
    (loop FOR exp = (read in nil nil)
          WHILE exp
          COLLECT exp)))

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

(defun @compile-file (input output)
 (with-open-file (out output :direction :output
                     :if-exists :supersede 
                     :element-type 'octet)
  (let ((*symbols* '())
        (*data* '()))
    ;; header
    (write-header out)

    ;; symbol-table
    (write-symbol-table out)
    
    ;; init-data
    (write-init-data out)
    
    ;; body
    (dolist (exp (@read2 input))
      (let ((*body* (@compile exp)))
        (write-body out))))))

(define-symbol-macro e (@compile (@read "fib.lisp")))
