(in-package :plint.parser)

(defun guess-expression-type (in)
  (case (@peek-char in)
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) :number)
    ((#\") :string)
    ((#\() :list)
    ((#\') :quote)
    ((#\#) :number-sign)
    (otherwise :symbol)))

(defun guess-number-signed-expression-type (in)
  (ecase (@peek-char in)
    ((#\t #\f) :boolean)
    ((#\\) :character)))

(defun parse-number (in)
  (let ((token (read-until-delimiter in)))
    `(:number ,(parse-integer token))))

(defun parse-string (in)
  (@read-char in)
  (labels ((recur (acc quote)
             (let ((c (@read-char in t)))
               (if quote
                   (recur (cons c acc) nil)
                 (case c
                   (#\" (coerce (nreverse acc) 'string))
                   (#\\ (recur acc t))
                   (otherwise (recur (cons c acc) nil)))))))
    `(:string ,(recur '() nil))))

(defun parse-symbol (in)
  (@peek-char in t) ; check eof
  (let ((token (read-until-delimiter in)))
    `(:symbol ,token)))

(defun parse-quote (in)
  (@read-char in)
  `(:quote ,(parse in)))

(defun parse-list (in)
  (@read-char in)
  (loop WHILE (char/= (progn (skip-white-space in)
                             (@peek-char in t))
                      #\))
        COLLECT (parse in) INTO list
        FINALLY 
        (@read-char in)
        (return `(:list ,list))))

(defun parse-character (in)
  (@read-char in) 
  `(:character ,(@read-char in t)))

(defun parse-boolean (in)
  `(:boolean ,(ecase (@read-char in)
                (#\t t)
                (#\f nil))))

(defun parse (in)
  (skip-white-space in)
  (ecase (guess-expression-type in)
    (:number (parse-number in))
    (:string (parse-string in))
    (:symbol (parse-symbol in))
    (:list   (parse-list in))
    (:quote  (parse-quote in))
    (:number-sign 
     (@read-char in) 
     (ecase (guess-number-signed-expression-type in)
       (:character (parse-character in))
       (:boolean   (parse-boolean in))))))
