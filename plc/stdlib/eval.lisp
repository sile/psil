(begin
 (define __int__ 1)
 (define __symbol__ 4)
 (define __undef__ 9)
 (define __apply__ 101)
 (define __symget__ 50)

 (define !cp-number (lambda (n)
   (flat-list __int__ (int->list n))))

 (define !cp-undef (lambda ()
   (flat-list __undef__)))

 (define !cp-apply (lambda (fn args)
   ;; TODO: handling tail call
   (flat-list (map compile args) (compile fn) __apply__ (length args))))

 (define !cp-symbol-value (lambda (sym)
   ;; TODO: handling local variable
   (let ((name (map char->integer (string->list (symbol->string sym)))))
     (flat-list __symbol__ (short->list (length name)) name __symget__))))

 (define !cp-pair (lambda (pair)
   (case (car pair)
     ((lambda) )
     ;; etc
     (else
      (!cp-apply (car pair) (cdr pair))))))

 (define !cp-symbol (lambda (sym)
   (!cp-symbol-value sym)))

 (define compile (lambda (exp)
   (case (type-of exp)
     ((number) (!cp-number exp))
     ((pair) (!cp-pair exp))
     ((symbol) (!cp-symbol exp))
     (else (!cp-undef)))))

 (define eval (lambda (exp . environment-specifier)
   (let ((bytecode-list (compile exp)))
     (__eval bytecode-list))))
 )