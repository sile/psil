(begin
 (define *whitespace* (list (integer->char 32)
                            (integer->char  9)
                            (integer->char 10)
                            (integer->char 13)))
                            
 (define !skip-whitespace (lambda (in)
   (if (memv (peek-char in) *whitespace*)
       (begin (read-char in)
              (!skip-whitespace in)))))
 
 (define !parse-port (lambda (in)
   (!skip-whitespace in)
   (let ((ch (read-char in)))
     ch)
   ))
 
 (define !parse-file (lambda (filepath)
   (call-with-input-file filepath (lambda (in) (!parse-port in)))))
 )
