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


;; for dev
(defun w (name bc)
  (pvm:write-bytecodes-to-file (format nil "/tmp/~a.bc" name) bc))


