(begin
 (define __int__ 1)

 (define !cp-number (lambda (n)
   (flat-list __int__ (int->list n))))

 (define compile (lambda (exp)
   (case (type-of exp)
     ((number) (!cp-number exp))
     (else (undef)))))

 (define eval (lambda (exp . environment-specifier)
   (let ((bytecode-list (compile exp)))
     (__eval bytecode-list))))
 )