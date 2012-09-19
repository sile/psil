(in-package :plint.parser)

(defun @peek-char (in &optional eof-error-p (eof-value #\Return))
  (peek-char nil in eof-error-p eof-value))

(defun @read-char (in &optional eof-error-p)
  (read-char in eof-error-p #\Return))

(defparameter *white-space-chars*
  '(#\Space #\Tab #\Newline #\Return))

(defparameter *delimiter-chars*
  (append *white-space-chars*
          '(#\( #\) #\" #\#)))

(defun skip-white-space (in)
  (loop WHILE (find (@peek-char in nil #\Null) *white-space-chars*)
        DO (@read-char in)))

(defun read-until-delimiter (in &optional (delimiters *delimiter-chars*))
  (loop WHILE (not (find (@peek-char in) delimiters))
        FOR c = (@read-char in)
        COLLECT c INTO list
        FINALLY (return (coerce list 'string))))
