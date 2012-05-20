(define-macro or (lambda exps
  (if (null? exps)
      #f
    (let ((exp (car exps))
          (rest (cdr exps))
          (it (gensym)))
      (list 'let (list (list it exp))
            (list 'if it it (cons 'or rest)))))))

