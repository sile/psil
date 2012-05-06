(begin
 (define repl (lambda ()
   (write-string "> ")
   (let ((exp (!parse-port (current-input-port))))
     ;; TODO: eval => print
     (if (not (eq? exp 'q))
         (repl)))))

 (repl)
 )