(begin
 (define repl (lambda ()
   (write-string "> ")
   (let ((exp (!parse-port (current-input-port))))
     (if (not (eq? exp 'q))
         (begin (write (eval exp))
                (repl))))))

 (repl)
 )