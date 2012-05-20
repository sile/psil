(begin
 (define __int__ 1)
 (define __string__ 2)
 (define __char__ 3)
 (define __symbol__ 4)
 (define __nil__ 5)
 (define __true__ 6)
 (define __false__ 7)
 (define __undef__ 9)
 (define __symget__ 50)
 (define __symset__ 51)

 (define __apply__ 101)
 (define __return__ 103)
 (define __fix_jump__ 152)
 (define __fix_jump_if__ 153)
 
 (define __drop__ 180)
 (define __lambda__ 201)
 (define __localget__ 202)
 (define __localset__ 203)
 (define __local_mkref__ 204)
 (define __local_refget__ 205)
 (define __local_refset__ 206)
 (define __local_toref__ 207)

 (define !cp-number (lambda (n)
   (flat-list __int__ (int->list n))))

 (define !cp-undef (lambda ()
   (flat-list __undef__)))

 (define !cp-null (lambda ()
   (flat-list __nil__)))

 (define !cp-apply (lambda (fn args env)
   ;; TODO: handling tail call
   (flat-list (map (lambda (a) (compile a env)) args) (compile fn env) __apply__ (length args))))

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

 (define !cp-intern (lambda (sym)
   (let ((name (map char->integer (string->list (symbol->string sym)))))
     (flat-list __symbol__ (short->list (length name)) name __symget__))))

 (define !cp-symbol-value (lambda (sym env)
   (let ((it (!find-local-bind sym env)))
     (if it
         (if (!local-bind-readonly? it)
             (flat-list __localget__ (!local-bind-index it))
           (flat-list __local_refget__ (!local-bind-index it)))
       (!cp-intern sym)))))

 (define !cp-set-nopush (lambda (var val initial env)
   (let ((it (!find-local-bind var env)))
     (if it
         (let ((op (if (!local-bind-readonly? it)
                       __localset__
                     (if initial
                         __local_mkref__
                       __local_refset))))
           (flat-list val op (!local-bind-index it)))
       (flat-list val (!cp-symbol-self var env) __symset__)))))

 (define !make-local-bind (lambda (var read-only? env)
   (list 'local var (!env-get-and-incr-local-var-index env) read-only?)))

 (define !local-bind-readonly? (lambda (bind) (cadddr bind)))
 (define !local-bind-index (lambda (bind) (caddr bind)))
 (define !local-bind-var (lambda (bind) (cadr bind)))
 (define !find-local-bind (lambda (var env)
   (let ((it (member-if (lambda (bind) 
                          (eq? var (!local-bind-var bind)))
                        (!env-get-bindings env))))
     (and it (car it)))))
 
 (define !cp-let (lambda (bindings body env)
   (let* ((body (cons 'begin body))  ; implicit body
          (vars (map car bindings))
          (closed-vars (intersection (cadr (!inspect body)) vars))
          (env (!env-toplevel env #f))
          (old-bindings (!env-get-bindings env))
          (new-bindings (append (map (lambda (var)
                                       (!make-local-bind var (not (memv var closed-vars)) env))
                                     vars)
                                old-bindings))
          (env (!env-bindings env new-bindings)))
     (flat-list 
      (map (lambda (bind)
             (let ((var (car bind))
                   (val (let ((env (!env-bindings env old-bindings)))
                          (compile (cadr bind) env))))
               (!cp-set-nopush var val #t env)))
           bindings)
      (compile body env)))))

 (define !adjust-args (lambda (args env)
   (map (lambda (a)
              (let ((it (!find-local-bind a env)))
                (if (!local-bind-readonly? it)
                    '()
                  (flat-list __local_toref__ (!local-bind-index it)))))
        args)))
                    
 ;; 可変長引数対策
 (define !normalize-args (lambda (args arity acc)
   (if (null? args)
       (list (reverse acc) arity #f)
     (if (pair? args)
         (!normalize-args (cdr args) (+ arity 1) (cons (car args) acc))
       (list (reverse (cons args acc)) (+ arity 1) #t)))))

 (define !cp-lambda (lambda (args body env toplevel?)
   (let* ((body (cons 'begin body))

          (tmp1 (!normalize-args args 0 '()))
          (args (car tmp1))
          (arity (cadr tmp1))
          (vararg? (caddr tmp1))

          (tmp2 (!inspect body))
          (free-vars (car tmp2))
          (mutable-free-vars (cadr tmp2))
          
          (binded-vars (map !local-bind-var (!env-get-bindings env)))
          (closing-vars (intersection free-vars
                                      (set-difference binded-vars args)))
          (closing-var-indices (map (lambda (v) (!local-bind-index (!find-local-bind v env)))
                                    closing-vars))
          (closed-args (intersection mutable-free-vars args))
          (env (!env-toplevel env toplevel?))
          (env (!env-reset-local-var-index env))
          (new-bindings (append (map (lambda (var) 
                                       (!make-local-bind var (not (memv var closed-args)) env))
                                     args)
                                (map (lambda (var) 
                                       (!make-local-bind var (!local-bind-readonly? (!find-local-bind var env)) env))
                                     closing-vars)
                                (!env-get-bindings env)))
          (env (!env-bindings env new-bindings)))

     (let ((body~ (flat-list (!adjust-args args env)
                             (compile body env)))
           (local-var-count (- (- (!env-get-local-var-index env) (length args)) (length closing-vars))))
       (flat-list (map (lambda (i) (flat-list __localget__ i)) (reverse closing-var-indices))
                  __lambda__ (length closing-vars) arity
                  local-var-count (if vararg? 1 0)
                  (int->list (+ 1 (length body~))) body~ __return__)))))

 (define !cp-inner-define (lambda (exps subsequent-exps env)
   (let ((var (car exps))
         (val (cadr exps)))
     (compile (append (list 'let (list (list var val))) subsequent-exps) env))))

 (define !cp-begin-impl (lambda (exp rest env)
   (if (and (not (!env-toplevel? env))
            (pair? exp)
            (eq? (car exp) 'define))
       (!cp-inner-define (cdr exp) rest env) ; TODO:
     (if (not (null? rest))
         (flat-list (compile exp env) __drop__ (!cp-begin-impl (car rest) (cdr rest) env))
       (compile exp env)))))

 (define !cp-begin (lambda (body env)
   (if (null? body)
       (!cp-undef)
     (!cp-begin-impl (car body) (cdr body) env))))

 (define !fixjump (lambda (offset) (flat-list __fix_jump__ (short->list offset))))
 (define !fixjump-if (lambda (offset) (flat-list __fix_jump_if__ (short->list offset))))

 (define !cp-if (lambda (exp then else env)
   (let* ((then~ (compile then env))
          (else~ (flat-list (compile else env) (!fixjump (length then~)))))
     (flat-list (compile exp env) (!fixjump-if (length else~)) else~ then~))))

 (define !cp-define (lambda (var val env)
   (flat-list (compile val env) (!cp-symbol-self var env) __symset__ (!cp-undef))))

 (define !cp-pair (lambda (pair env)
   (if (!env-quote? env)
       (!cp-list pair env)
     (case (car pair)
       ((quote) (!cp-quote (cdr pair) env))
       ((begin) (!cp-begin (cdr pair) env))
       ((lambda) (!cp-lambda (cadr pair) (cddr pair) env #f))
       ((toplevel-lambda) (!cp-lambda (cadr pair) (cddr pair) env #t))
       ((let) (let ((bindings (car (cdr pair)))
                    (body     (cdr (cdr pair))))
                (!cp-let bindings body env)))
       ((if) (if (= (length (cdr pair)) 2)
                 (!cp-if (cadr pair) (caddr pair) '(undef) env)
               (!cp-if (cadr pair) (caddr pair) (cadddr pair) env)))
       ((define) (!cp-define (cadr pair) (caddr pair) env))
       ((set!)) ; TODO:
       ;; TODO: macro

       ;; etc
       (else
        (!cp-apply (car pair) (cdr pair) env))))))

 (define !cp-symbol (lambda (sym env)
   (!cp-symbol-value sym env)))

 (define !cp-char (lambda (ch)
   (flat-list __char__ (int->list (char->integer ch)))))
 
 (define !cp-string (lambda (str)
   (let ((str (map char->integer (string->list str))))
     (flat-list __string__ (int->list (length str)) str))))

 (define !cp-boolean (lambda (bool)
   (if bool (flat-list __true__) (flat-list __false__))))

 (define !init-env (lambda ()
   '(
     (quote . #f)
     (toplevel . #t)
     (bindings . ())
     (local-var-index . 0)
     )))
 
 (define !env-reset-local-var-index (lambda (env)
   (cons (cons 'local-var-index 0) env)))

 (define !env-get-local-var-index (lambda (env)
   (let* ((x (assv 'local-var-index env))
          (n (cdr x)))
     n)))

 (define !env-get-and-incr-local-var-index (lambda (env)
   (let* ((x (assv 'local-var-index env))
          (n (cdr x)))
     (set-cdr! x (+ n 1))
     n)))

 (define !env-quote (lambda (env bool)
   (cons (cons 'quote bool) env)))

 (define !env-quote? (lambda (env)
   (let ((x (assv 'quote env)))
     (and x (cdr x)))))

 (define !env-toplevel (lambda (env bool)
   (cons (cons 'toplevel bool) env)))

 (define !env-toplevel? (lambda (env)
   (let ((x (assv 'toplevel env)))
     (and x (cdr x)))))

 (define !env-get-bindings (lambda (env)
   (cdr (assv 'bindings env))))
 
 (define !env-bindings (lambda (env bindings)
   (cons (cons 'bindings bindings) env)))

 ;;
 (define !state-binded-vars-pair (lambda (state) (assv 'binded-vars state)))
 (define !state-binded-vars (lambda (state) (cdr (!state-binded-vars-pair state))))
 
 (define !state-mutable-free-vars-pair (lambda (state) (assv 'mutable-free-vars state)))
 (define !state-mutable-free-vars (lambda (state) (cdr (!state-mutable-free-vars-pair state))))
 
 (define !state-free-vars-pair (lambda (state) (assv 'free-vars state)))
 (define !state-free-vars (lambda (state) (cdr (!state-free-vars-pair state))))
 
 (define !inspect (lambda (exp)
   (let ((state (list (cons 'free-vars '())
                      (cons 'mutable-free-vars '())
                      (cons 'binded-vars '()))))
     (!inspect-impl exp state)
     (list (!state-free-vars state)
           (!state-mutable-free-vars state)))))

 (define !inspect-impl (lambda (exp state)
   (case (type-of exp)
     ((symbol) (!inspect-symbol exp state))
     ((pair)   (!inspect-list exp state)))))

 (define !inspect-symbol (lambda (exp state)
   (if (and (not (memv exp (!state-binded-vars state)))
            (not (memv exp (!state-free-vars state))))
       (set-cdr! (!state-free-vars-pair state) 
                 (cons exp (!state-free-vars state))))))

 (define !inspect-list (lambda (pair state)
   (let ((hd (car pair))
         (tl (cdr pair)))
     (case hd
       ((quote))
       ((if begin) (for-each (lambda (exp) (!inspect-impl exp state)) tl))
       ((lambda) (let* ((args (car tl))
                        (body (cdr tl))
                        (new-binded-vars (append (!normalize-args args 0 '())
                                                 (!state-binded-vars state)))
                        (new-state (cons (cons 'binded-vars new-binded-vars)
                                         state)))
                   (!inspect-impl (cons 'begin body) new-state)))
       ((define) (let ((var (car tl))
                       (val (cadr tl)))
                   (!inspect-impl val state)
                   ;; TODO: toplevelかどうかを考慮する
                   (set-cdr! (!state-binded-vars-pair state)
                             (cons var (!state-binded-vars state)))))
       ((set!) (let ((var (car tl))
                     (val (cadr tl)))
                 (!inspect-impl val state)
                 (if (not (memv var (!state-binded-vars state)))
                     (let ((free-vars-pair (!state-free-vars-pair state))
                           (mutable-free-vars-pair (!state-mutable-free-vars-pair state)))
                       (set-cdr! free-vars-pair (cons var (cdr free-vars-pair)))
                       (set-cdr! mutable-free-vars-pair (cons var (cdr mutable-free-vars-pair)))))))
       (else (for-each (lambda (exp) (!inspect-impl exp state)) tl))))))

 (define compile (lambda (exp env)
   (case (type-of exp)
     ((null)   (!cp-null))
     ((number) (!cp-number exp))
     ((boolean)(!cp-boolean exp))
     ((char)   (!cp-char exp))
     ((string) (!cp-string exp))
     ((pair)   (!cp-pair exp env))
     ((symbol) (if (!env-quote? env)
                   (!cp-symbol-self exp env)
                 (!cp-symbol exp env)))
     (else     (!cp-undef)))))

 (define eval (lambda (exp . environment-specifier)
   (let ((bytecode-list (compile (list (list 'toplevel-lambda '()  exp)) ; toplevel-lambda
                                 (!init-env))))
     (__eval bytecode-list))))
 )