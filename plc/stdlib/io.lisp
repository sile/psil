(begin
 (define call-with-input-file (lambda (filepath proc)
                                (let ((in (open-input-file filepath)))
                                  (let ((result (proc in)))
                                    (close-input-port in)
                                    result))))

 (define call-with-output-file (lambda (filepath proc)
                                (let ((in (open-output-file filepath)))
                                  (let ((result (proc in)))
                                    (close-output-port in)
                                    result))))

 (define with-input-from-file 
   (lambda (filepath thunk)
     (call-with-input-file 
      filepath
      (lambda (in)
        (let ((old current-input))
          (set! current-input in)
          (let ((result (thunk)))
            (set! current-input old)
            result))))))

 (define with-output-to-file 
   (lambda (filepath thunk)
     (call-with-output-file 
      filepath
      (lambda (in)
        (let ((old current-output))
          (set! current-output in)
          (let ((result (thunk)))
            (set! current-output old)
            result))))))

 (define newline (lambda port
                   (let ((nl (integer->char 10)))
                     (if (pair? port)
                         (write-char nl (car port))
                       (write-char nl)))))

 (define !get-output-port (lambda (maybe-port)
                            (if (null? maybe-port)
                                (current-output-port)
                              (car maybe-port))))

 (define write-string (lambda (str . port)
                        (let ((port (!get-output-port port)))
                          (for-each (lambda (ch) (write-char ch port))
                                    (string->list str)))))

 (define write-byte-list (lambda (lst)
   (for-each (lambda (byte) (write-byte byte))
             lst)))

 (define read (lambda port
   (let ((port (if (null? port) (current-input-port) port)))
     (!parse-port port))))

 (define load-read-contents (lambda ()
   (let ((exp (read)))
     (if (eof-object? exp)
         '()
       (cons exp (load-read-contents))))))

 (define load (lambda (filepath)
                (with-input-from-file 
                 filepath
                 (lambda ()
                   (let ((exps (cons 'begin (load-read-contents))))
                     (eval exps))))))
 )
