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

 (define newline (lambda ()
                   (let ((nl (integer->char 10)))
                     (write-char nl))))
 )

