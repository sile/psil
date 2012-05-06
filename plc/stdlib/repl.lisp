(begin
 (define repl (lambda ()
   (write-string "> ")
   (!parse-port (current-input-port))))

 (repl)
 )