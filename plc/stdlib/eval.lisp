(begin
 (define compile (lambda (exp)
   ))

 (define eval-bytecode (lambda (bytecode)
   ))

 (define eval (lambda (exp . environment-specifier)
   (let ((bytecode (compile exp)))
     (eval-bytecode bytecode))))
 )