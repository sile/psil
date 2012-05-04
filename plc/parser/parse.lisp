(in-package :plcp)

(defparameter *whitespace* '(#\Space #\Tab #\Newline #\Return))
(defparameter *delimiter* (append *whitespace* '(#\' #\" #\# #\( #\) #\Null)))

(defun skip-whitespace (in)
  (loop WHILE (member (peek-ch in) *whitespace*)
        DO (read-ch in)))

(defun read-until-delimiter (in)
  (loop FOR c = (char-upcase (read-ch in))
        UNTIL (member c *delimiter*)
        COLLECT c INTO list
        FINALLY 
        (unless (char= c #\Null)
          (unread-char c in))
        (return (coerce list 'string))))

(defun to-symbol (str)
  (intern (map 'string #'char-upcase str) :keyword))

(defun read-symbol (in)
  (to-symbol (read-until-delimiter in)))

(defun char-type (c)
  (case c
    (#\Null :eof)
    (#\" :string)
    (#\( :list)
    (#\) :close)
    (#\' :quote)
    (#\# :boolean-or-char)
    (otherwise
     (cond ((and (char<= #\0 c) (char<= c #\9))
            :maybe-number)
           (t
            :symbol)))))

(defun @parse-boolean (in)
  (read-ch in) ; eat #\#
  (ecase (read-ch in)
    ((#\t #\T) :|true|)
    ((#\f #\F) :|false|)))

(defun @parse-string (in)
  (read-ch in) ; eat #\"
  (loop FOR c = (read-ch in)
        UNTIL (prog1 (char= c #\")
                (assert (char/= c #\NULL) () "delect EOS"))
    COLLECT c INTO list
    FINALLY (return (coerce list 'string))))

(defun @parse-quote (in)
  (read-ch in) ; eat #\'
  `(:|quote| ,(parse in)))

(defun @parse-symbol (in)
  (read-symbol in))

(defun @parse-list (in)
  (read-ch in) ; eat #\(
  (loop FOR x = (parse in)
        UNTIL (eq x :|)|)
        COLLECT x))

(defun @parse-char (in)
  (read-ch in) ; eat #\#
  (read-ch in) ; eat #\\
  (read-ch in))

(defun @parse-boolean-or-char (in)
  (read-ch in)
  (if (prog1 (char= (peek-ch in) #\\) (unread-char #\# in))
      (@parse-char in)
    (@parse-boolean in)))

(defun @parse-number-or-symbol (in)
  (let ((x (read-until-delimiter in)))
    (if (every #'digit-char-p x)
        (parse-integer x)
      (to-symbol x))))

(defun parse (in)
  (skip-whitespace in)
  (ecase (char-type (peek-ch in))
    (:eof :|eof|)
    (:string (@parse-string in))
    (:list (@parse-list in))
    (:close (read-ch in) :|)|)
    (:quote (@parse-quote in))
    (:symbol (@parse-symbol in))
    (:boolean-or-char (@parse-boolean-or-char in))
    (:maybe-number (@parse-number-or-symbol in))))
