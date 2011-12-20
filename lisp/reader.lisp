(defpackage psil.reader
  (:use :common-lisp)
  (:shadow :common-lisp read)
  (:export read))
(in-package :psil.reader)

#|
構成要素:
 - 数値
  - 整数: 11
  - 実数: 0.0
 - 文字列: ""
 - シンボル: xxx
 - リスト(コンス): ()
  - nil
 - 配列: []

 - quote

 - コメント
|#

(defun maybe-object-type (in)
  (case (peek-char nil in) 
    (#\( 'cons)
    (#\[ 'array)
    (#\" 'string)
    (#\' 'quote)
    (otherwise
     (if (digit-char-p (peek-char nil in))
         'symbol-or-number
       'symbol))))

(defparameter *whitespace-chars* '(#\Space #\Tab #\Return #\Newline))
(defparameter *delimiter-chars* `(#\[ #\( #\] #\) #\' . ,*whitespace-chars*))

(defun skip-whitespace (in)
  (loop WHILE (member (peek-char t in) *whitespace-chars*)
        DO (read-char in)))

(defun read-until (ch in)
  (loop WHILE (char/= (peek-char nil in) ch)
        COLLECT (read-char in) INTO list
        FINALLY (return (coerce list 'string))))

(defun read-until-whitespace (in)
  (loop UNTIL (member (peek-char nil in nil #\Newline) *whitespace-chars*)
        COLLECT (read-char in) INTO list
        FINALLY (return (coerce list 'string))))

(defun read-until-delimiter (in)
  (loop WITH first = (read-char in)
        UNTIL (or (member first *delimiter-chars*) ;; XXX:
                  (member (peek-char nil in nil #\Newline) *delimiter-chars*))
        COLLECT (read-char in) INTO list
        FINALLY (return (coerce (cons first list) 'string))))

(defun read-string (in)
  (read-char in)     ; eat #\"
  (prog1 (read-until #\" in)
    (read-char in))) ; eat #\"

(defun read-symbol (in)
  (read-until-delimiter in))

(defun read-quote (in)
  (read-char in)  ; eat #\'
  (read in))

(defun read-array (in)
  (read-char in) ; eat #\[
  (loop FOR (type value) = (read in)
        UNTIL (equal value "]")
        COLLECT (list type value)))

(defun read-cons (in)
  (read-char in) ; eat #\(
  (labels ((recur ()
             (destructuring-bind (type value) (read in)
               (if (equal value ")")
                   (list :nil nil)
                 (list :cons (list (list type value)
                                   (recur)))))))
    (recur)))

(defun read-symbol-or-number (in)
  (let ((str (read-until-delimiter in)))
    (if (every #'digit-char-p str)
        (list :number (parse-integer str)) ;; XXX: float
      (list :symbol str))))

(defun read (in)
  (skip-whitespace in)
  (case (maybe-object-type in)
    (string (list :string (read-string in)))
    (cons (read-cons in))
    (array (list :array (read-array in)))
    (symbol (list :symbol (read-symbol in)))
    (symbol-or-number (read-symbol-or-number in))
    (quote (list :quote (read-quote in)))))
