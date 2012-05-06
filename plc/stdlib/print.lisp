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

 (define write (lambda (x)
   (case (type-of x)
     ((number) (!write-number x))
     (else (write-string "Not Implemented")))
   (newline))) ; XXX
 )