(begin
 (define *char-return* (integer->char 13))
 (define *char-newline* (integer->char 10))
 
 (define *whitespace* (list (integer->char 32)
                            (integer->char  9)
                            *char-newline*
                            *char-return*))
 
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

 (define !read-string (lambda (in escape)
   (if (and (not escape) 
            (char= (peek-char in) #\"))
       (begin (read-char in)
              '())
     (let* ((ch (read-char in))
            (escape (and (not escape)
                         (char= ch #\\))))
       (if escape
           (!read-string in escape)
         (cons ch (!read-string in escape)))))))

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

 (define !parse-string (lambda (in)
   (read-char in) ; eat #\"
   (list->string (!read-string in #f))))

 (define !skip-comment-line (lambda (in)
   (if (not (memv (read-char in) (list *char-return* *char-newline*)))
       (!skip-comment-line in))))

 (define !parse-list-impl (lambda (in)
   (if (char= (peek-char in) #\))
       (begin (read-char in)
              '())
     (cons (!parse-port in)
           (!parse-list-impl in)))))

 (define !parse-list (lambda (in)
   (read-char in) ; eat #\)
   (!parse-list-impl in)))

 (define !parse-boolean-or-char (lambda (in)
   (read-char in) ; eat #\#
   (case (read-char in)
     ((#\\) (!parse-char in))
     ((#\t) #t)
     ((#\f) #f)
     (else (undef)))))

 (define !parse-port (lambda (in)
   (!skip-whitespace in)
   (let ((ch (peek-char in)))
     (case (!char-type ch)
       ((@eof) (undef))
       ((@comment) (!skip-comment-line in) (!parse-port in))
       ((@string) (!parse-string in))
       ((@list) (!parse-list in))
       ((@quote) (!parse-quote in))
       ((@symbol) (!parse-symbol in))
       ((@boolean-or-char) (!parse-boolean-or-char in))
       ((@maybe-number) 9)))))
 
 (define !parse-file (lambda (filepath)
   (call-with-input-file filepath (lambda (in) (!parse-port in)))))
 )
