(begin
 (define call-with-input-file (lambda (filepath proc)
                                (let ((in (open-input-file filepath)))
                                  (let ((result (proc in)))
                                    (close-input-port in)
                                    result))))

 )