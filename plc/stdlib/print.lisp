(begin
 
 (define digit->char (lambda (n)
   (integer->char (+ n (char->integer #\0)))))

 (define !write-number-impl (lambda (n)
   (if (< n 10)
       (write-char (digit->char n))
     (begin
      (!write-number-impl (/ n 10))
      (write-char (digit->char (modulo n 10)))))))
 
 (define !write-number (lambda (n)
   (if (< n 0)
       (begin (write-char #\-)
              (!write-number-impl (* -1 n)))
     (!write-number-impl n))))

 (define !write-symbol (lambda (sym)
   (write-string (symbol->string sym))))

 (define !write-procedure (lambda (proc)
   (write-string "<PROC>")))

 (define !write-pair-impl (lambda (x)
   (if (pair? x)
       (begin (write-no-nl (car x))
              (if (pair? (cdr x))
                  (write-string " "))
              (!write-pair-impl (cdr x)))
     (if (not (null? x))
         (begin (write-string " . ")
                (write-no-nl x))))))

 (define !write-pair (lambda (pair)
   (write-string "(")
   (!write-pair-impl pair)
   (write-string ")")))

 (define write-no-nl (lambda (x)
   (case (type-of x)
     ((null)   (write-string "()"))
     ((number) (!write-number x))
     ((symbol) (!write-symbol x))
     ((pair)   (!write-pair x))
     ((procedure) (!write-procedure x))
     (else (if (eq x (undef))
               (write-string "<UNDEF>")
             (write-string "Not Implemented"))))))

 (define write (lambda (x)
   (write-no-nl x)
   (newline)))
 )