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

      (defun skip-whitespace (in)
        )

      (defun parse-from-stream (in)
        (skip-whitespace in)
        )
      
      (parse-from-file "/tmp/s.lisp")
      )))



;; for dev
(defun w (name bc)
  (pvm:write-bytecodes-to-file (format nil "/tmp/~a.bc" name) bc))


