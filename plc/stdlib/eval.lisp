(begin
 (define __int__ 1)
 (define __symbol__ 4)
 (define __nil__ 5)
 (define __undef__ 9)
 (define __apply__ 101)
 (define __symget__ 50)

 (define !cp-number (lambda (n)
   (flat-list __int__ (int->list n))))

 (define !cp-undef (lambda ()
   (flat-list __undef__)))

 (define !cp-null (lambda ()
   (flat-list __nil__)))

 (define !cp-apply (lambda (fn args env)
   ;; TODO: handling tail call
   (flat-list (map (lambda (a) (compile a env)) args) (compile fn env) __apply__ (length args))))

 (define !cp-symbol-value (lambda (sym env)
   ;; TODO: handling local variable
   (let ((name (map char->integer (string->list (symbol->string sym)))))
     (flat-list __symbol__ (short->list (length name)) name __symget__))))

 (define !cp-symbol-self (lambda (sym env)
   ;; TODO: handling local variable
   (let ((name (map char->integer (string->list (symbol->string sym)))))
     (flat-list __symbol__ (short->list (length name)) name))))

 (define !cp-quote (lambda (exps env)
   (let ((exp (car exps)))
     (compile exp (!env-quote env #t)))))

 (define !cp-list-impl (lambda (pair)
   (if (pair? pair)
       (list 'cons (list 'quote (car pair)) (!cp-list-impl (cdr pair)))
     (list 'quote pair))))

 (define !cp-list (lambda (pair env)
   (compile (!cp-list-impl pair) (!env-quote env #f))))

 (define !cp-pair (lambda (pair env)
   (case (car pair)
     ((quote) (!cp-quote (cdr pair) env))
     ((lambda) )
     ;; etc
     (else
      (if (!env-quote? env)
          (!cp-list pair env)
        (!cp-apply (car pair) (cdr pair) env))))))

 (define !cp-symbol (lambda (sym env)
   (!cp-symbol-value sym env)))

 (define !init-env (lambda ()
   '(
     (quote . #f)
     )))

 (define !env-quote (lambda (env bool)
   (cons (cons 'quote bool) env)))

 (define !env-quote? (lambda (env)
   (let ((x (assv 'quote env)))
     (and x (cdr x)))))

 (define compile (lambda (exp env)
   (case (type-of exp)
     ((null)   (!cp-null))
     ((number) (!cp-number exp))
     ((pair)   (!cp-pair exp env))
     ((symbol) (if (!env-quote? env)
                   (!cp-symbol-self exp env)
                 (!cp-symbol exp env)))
     (else     (!cp-undef)))))

 (define eval (lambda (exp . environment-specifier)
   (let ((bytecode-list (compile exp (!init-env))))
     (__eval bytecode-list))))
 )