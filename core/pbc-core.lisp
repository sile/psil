(in-package :pbc-core)
;; pbootとかの方が適切?

;; [coreバイトコードが提供する関数]
;; - parse
;; - compile

(defun gen-basic-io-bc ()
  (compile-bytecode
   '(progn 
      (defun open-input-file (filename)
        ($open filename $o_rdonly #o666))

      (defun close-input-port (port)
        ($close port))
      
      (defun read-char (port)
        ($code-char ($read-byte port)))

      (defun eof? (port)
        ($eof-p port))
      )))

(defun gen-parse-bc ()
  (compile-bytecode
   '(progn
      (defun parse-from-file (path)
        (parse-from-stream (open-input-file path)))

      (defun read-skip-whitespace (in)
        (let ((c (read-char in)))
          (if (! ^or 
                 ($char= c #\Space)
                 ($char= c #\Tab)
                 ($char= c #\Newline)
                 ($char= c #\Return))
              (read-skip-whitespace in)
            c)))

      (defun parse-from-stream (in)
        (let ((c (read-skip-whitespace in)))
          (! ^cond
             (($char= c #\Null) nil)
             (($char= c #\() 'list)
             ((and ($char<= $\0 c) ($char<= c #\9)) 'integer)
             (($char= c #\') 'quote)
             (t 'symbol))))
      
      (parse-from-file "/tmp/s.lisp")
      )))



;; for dev
(defun w (name bc)
  (pvm:write-bytecodes-to-file (format nil "/tmp/~a.bc" name) bc))
