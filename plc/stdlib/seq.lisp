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