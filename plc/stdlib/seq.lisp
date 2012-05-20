(begin
 (define cadr (lambda (lst) (car (cdr lst))))
 (define caddr (lambda (lst) (cadr (cdr lst))))
 (define cadddr (lambda (lst) (caddr (cdr lst))))
 (define caddddr (lambda (lst) (cadddr (cdr lst))))

 (define memv (lambda (obj list)
                (if (pair? list)
                    (if (eqv? obj (car list))
                        list
                      (memv obj (cdr list)))
                  #f)))

 (define member-if (lambda (fn list)
                     (if (pair? list)
                         (if (fn (car list))
                             list
                           (member-if fn (cdr list)))
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

 (define string->list-impl (lambda (str len i)
   (if (= i len)
       '()
     (cons (string-ref str i)
           (string->list-impl str len (+ i 1))))))

 (define string->list (lambda (str)
   (string->list-impl str (string-length str) 0)))

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
   (let ((args (cons arg args)))
     (__apply fn (append (butlast args)
                         (car (last args)))))))

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
 
 (define for-each (lambda (fn lst . lsts)
    (apply map fn lst lsts)
    (undef)))

 (define not (lambda (x) (if x #f #t)))

 (define flat-list-impl (lambda (xs)
   (if (null? xs)
       '()
     (if (pair? (car xs))
         (append (flat-list-impl (car xs))
                 (flat-list-impl (cdr xs)))
       (if (null? (car xs))
           (flat-list-impl (cdr xs))
         (cons (car xs)
               (flat-list-impl (cdr xs))))))))

 (define flat-list (lambda xs
   (flat-list-impl xs)))
 
 (define range-list (lambda (start end)
   (if (> start end)
       '()
     (cons start (range-list (+ start 1) end)))))

 (define reverse-impl (lambda (lst acc)
   (if (null? lst)
       acc
     (reverse-impl (cdr lst) (cons (car lst) acc)))))

 (define reverse (lambda (lst)
   (reverse-impl lst '())))

 (define int->list (lambda (n) ; bit-endian
   (reduce (lambda (acc i)
             (let ((offset (* i 8)))
               (cons (bit-field n offset (+ offset 8)) acc)))
           '()
           (range-list 0 3))))

 (define short->list (lambda (n) ; bit-endian
   (reduce (lambda (acc i)
             (let ((offset (* i 8)))
               (cons (bit-field n offset (+ offset 8)) acc)))
           '()
           (range-list 0 1))))

 (define assv (lambda (obj lst)
   (if (null? lst)
       #f
     (if (eqv? obj (car (car lst)))
         (car lst)
       (assv obj (cdr lst))))))

 (define intersection (lambda (lst1 lst2)
   (if (null? lst1)
       '()
     (if (memv (car lst1) lst2)
         (cons (car lst1) (intersection (cdr lst1) lst2))
       (intersection (cdr lst1) lst2)))))
 )
