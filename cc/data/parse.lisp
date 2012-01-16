;;;;;;;;;;;;;;
;;; parser ;;;
(defun @parse-file (path)
  (let ((in (open/r path)))
    (if (null in)
        nil
      (@parse in))))

(defun @parse (in)
  (let ((type (maybe-object-type in)))
    (if (eq type 'cons)
        (@parse-cons in)
      (if (eq type 'string)
          (@parse-string in)
        (if (eq type 'quote)
            (@parse-quote in)
          (if (eq type 'symbol)
              (@parse-symbol in)
            (@parse-symbol-or-integer in)))))))

(setq *whitespaces* '(32 ; space
                      9  ; tab
                      10 ; newline
                      13 ; return
                      ))

(setq *byte-stock* nil)
(defun @peek-byte (in)
  (@skip-whitespace in)
  (if (null *byte-stock*)
      (setq *byte-stock* (read-byte in)))
  *byte-stock*)

(defun @peek-byte2 (in)
  (if (null *byte-stock*)
      (setq *byte-stock* (read-byte in)))
  *byte-stock*)

(defun @read-byte (in)
  (if *byte-stock*
      (let ((n *byte-stock*))
        (setq *byte-stock* nil)
        n)
    (read-byte in)))

(defun @eos (in)
  (null (@peek-byte in)))

(defun @skip-whitespace (in)
  (if (find-n (@peek-byte2 in) *whitespaces*)
      (progn (@read-byte in)
             (@skip-whitespace in))))

(defun find-n (num list)
  (if (null list)
      nil
    (if (= num (car list))
        t
      (find-n num (cdr list)))))

(defun maybe-object-type (in)
  (let ((n (@peek-byte in)))
    (if (= n 40) ; #\(
        'cons
      (if (= n 34) ; #\"
          'string
        (if (= n 39) ; #\'
            'quote
          (if (digit-char-p n)
              'symbol-or-integer
            'symbol))))))

(defun digit-char-p (n)
  (< 47 n 58)) ; #\0 - #\9

(defun @parse-cons (in)
  (@read-byte in) ; each #\(
  
  ;; XXX: => (recur-let  ) ?
  (let-rec ((recur (lambda ()
                     (if (= (@peek-byte in) 41) ; #\)
                         (progn (@read-byte in) nil)
                       (cons (@parse in) (recur))))))
    (recur)))

(setq *delimiters* (append '(91 40 93 41 39) *whitespaces*))

(defun @parse-symbol (in)
  (intern (string-upcase (list-to-string (@read-until *delimiters* in)))))

(defun @read-until (bag in)
  (if (find-n (@peek-byte2 in) bag)
      nil
    (cons (@read-byte in) (@read-until bag in))))

(defun @parse-string (in)
  (@read-byte in) ; eat #\"
  (let ((s (list-to-string (@read-until '(34) in))))
    (@read-byte in) ; eat #\"
    s))

(defun @parse-quote (in)
  (@read-byte in) ; eat #\'
  (list 'quote (@parse in)))

(defun @parse-symbol-or-integer (in)
  (let ((bytes (@read-until *delimiters* in)))
    (if (every digit-char-p bytes)
        (parse-integer (list-to-string bytes))
      (intern (string-upcase (list-to-string bytes))))))

#+C
(labels ((n (lambda (x)
              (+ x x))))
  (n 10))


(macro-let ((n (show 1)))
  n n n)

;;(setq n (symbol-macro (+ 1 2)))

;;(list n n)

(let-rec ((fib (lambda (x)
                 (if (< x 2)
                     x
                   (+ (fib (- x 2)) (fib (- x 1)))))))
  (fib 10))


(@parse-file "data/fib.lisp")

;(or nil 1 2)

;(mapcar2 + '(1 2 3) '(2 3 4))