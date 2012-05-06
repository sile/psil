(begin
 (define *whitespace* (list (integer->char 32)
                            (integer->char  9)
                            (integer->char 10)
                            (integer->char 13)))
 
 (define *delimiters* (append *whitespace*
                              (list #\' #\" #\# #\( #\) (integer->char 0))))

 (define !skip-whitespace (lambda (in)
   (if (memv (peek-char in) *whitespace*)
       (begin (read-char in)
              (!skip-whitespace in)))))
 
 (define !read-until-delimiter (lambda (in)
   (if (memv (peek-char in) *delimiters*)
       '()
     (cons (read-char in) (!read-until-delimiter in)))))

 (define !char-type (lambda (c)
   (case c
     ((#\;) '@comment)
     ((#\") '@string)
     ((#\() '@list)
     ((#\)) '@list-close)
     ((#\') '@quote)
     ((#\#) '@boolean-or-char)
     (else 
      (if (eqv? c (integer->char 0))
          '@eof
        (if (or (and (char<= #\0 c) (char<= c #\9))
                (char= #\- c) (char= #\+ c))
            '@maybe-number
          '@symbol))))))

 (define !parse-symbol (lambda (in)
   (string->symbol (list->string (map char-upcase (!read-until-delimiter in))))))

 (define !parse-quote (lambda (in)
   (read-char in) ; eat #\'
   (cons 'quote (!parse-port in))))

 (define !parse-port (lambda (in)
   (!skip-whitespace in)
   (let ((ch (peek-char in)))
     (case (!char-type ch)
       ((@eof) 1)
       ((@comment) 2)
       ((@string) 3)
       ((@list) 4)
       ((@list-close) 5)
       ((@quote) (!parse-quote in))
       ((@symbol) (!parse-symbol in))
       ((@boolean-or-char) 8)
       ((@maybe-number) 9)))))
 
 (define !parse-file (lambda (filepath)
   (call-with-input-file filepath (lambda (in) (!parse-port in)))))
 )
