(begin
 (define type-of (lambda (x)
   (if (boolean? x)
       'boolean
     (if (pair? x)
         'pair
       (if (symbol? x)
           'symbol
         (if (number? x)
             'number
           (if (char? x)
               'char
             (if (string x)
                 'string
               (if (port? x)
                   'port
                 (if (procedure? x)
                     'procedure
                   (if (null? x)
                       'null
                     (undef))))))))))))
 )