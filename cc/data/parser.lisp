
(defun @read (path)
  (with-open-file (in path)
    (read in)))

(defun @read2 (path)
  (with-open-file (in path)
    (loop FOR exp = (read in nil nil)
          WHILE exp
          COLLECT exp)))

;; TODO: 同じS式は同一参照を保持するような仕組みを作って、サイズを節約する
(defun @compile (exp)
  (etypecase exp
    (fixnum `(:integer ,exp))
    (string `(:string ,exp))
    (symbol `(:symbol ,(symbol-name exp)))
    (cons (if (eq (car exp) 'quote)
              `(:quote ,(@compile (second exp)))
            `(:list ,(mapcar (lambda (e)
                               (@compile e))
                             exp))))))

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

(define-symbol-macro d (progn (@compile-file "stdlib.lisp" "stdlib.bin")
                              (@compile-file "parse.lisp" "parse.bin")
                              (@compile-file "repl.lisp" "repl.bin")
                              ))
