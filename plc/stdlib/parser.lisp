(begin
 (define !skip-whitespace (lambda (in)
   
   ))
 
 (define !parse-port (lambda (in)
   in
   ))
 
 (define !parse-file (lambda (filepath)
   (call-with-input-file filepath (lambda (in) (!parse-port in)))))
 )
