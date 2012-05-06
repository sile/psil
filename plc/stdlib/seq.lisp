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

 (define butlast (lambda (lst)
   (if (null? lst)
       '()
     (if (null? (cdr lst))
         '()
       (cons (car lst)
             (butlast (cdr lst)))))))

 (define last (lambda (lst)
   (if (null? lst)
       '()
     (if (null? (cdr lst))
         lst
       (last (cdr lst))))))

 (define apply (lambda (fn arg . args)
   (let ((args1 (cons arg args)))
     (__apply fn (append (butlast args1)
                         (car (last args1)))))))

 (define any (lambda (fn lst)
   (if (null? lst)
       #f
     (or (fn (car lst))
         (any fn (cdr lst))))))

 (define map1 (lambda (fn lst)
   (if (null? lst)
       '()
     (cons (fn (car lst))
           (map1 fn (cdr lst))))))

 (define mapN (lambda (fn lsts)
   (if (any null? lsts)
       '()
     (cons (apply fn (map1 car lsts))
           (mapN fn (map1 cdr lsts))))))

 (define map (lambda (fn lst . lsts)
   (if (null? lsts)
       (map1 fn lst)
     (mapN fn (cons lst lsts)))))
 )