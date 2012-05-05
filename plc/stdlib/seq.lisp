(begin
 (define memv (lambda (obj list)
                (if (pair? list)
                    (if (eqv? obj (car list))
                        list
                      (memv obj (cdr list)))
                  #f)))

 (define length (lambda (list)
                  (if (pair? list)
                      (+ 1 (length (cdr list)))
                    0)))
 
 
 (define append2 (lambda (lst1 lst2)
                   (if (null? lst1)
                       lst2
                     (cons (car lst1) (append2 (cdr lst1) lst2)))))

 (define append (lambda lists
                  (if (null? lists)
                      lists
                    (reduce (lambda (acc lst)
                              (append2 acc lst))
                            (car lists)
                            (cdr lists)))))

 (define reduce (lambda (fn acc list)
                  (if (pair? list)
                      (reduce fn (fn acc (car list)) (cdr list))
                    acc)))

 (define list->string (lambda (lst)
                        (let* ((len (length lst))
                               (str (make-string len)))
                          (reduce (lambda (i ch)
                                    (string-set! str i ch)
                                    (+ i 1))
                                  0
                                  lst)
                          str)))
 )