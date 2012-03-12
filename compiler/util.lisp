(in-package :pc)

(defun int-to-bytes (n)
  (loop FOR i FROM 3 DOWNTO 0 
        COLLECT (ldb (byte 8 (* i 8)) n)))

(defun short-to-bytes (n)
  (loop FOR i FROM 1 DOWNTO 0 
        COLLECT (ldb (byte 8 (* i 8)) n)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (s) 
		   `(,s (gensym)))
		 syms)
     ,@body))

(defmacro a.if (expr consequent &optional alternative)
  `(let ((it ,expr))
     (if it
	 ,consequent
       ,alternative)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun s (&rest args)
    "ARGSを連接した文字列に変換する"
    (with-output-to-string (s)
      (dolist (a args)
	(typecase a
	  (string    (write-string a s))
	  (character (write-char a s))
	  (otherwise (princ a s))))))

  (defun formalize-letargs (args)
    (mapcar (lambda (a) (if (atom a) (list a) a)) args))

  (defun symb (&rest args)
    (intern (apply #'s args)))

  (defun pop-symbol (sym &optional (n 1))
    (intern (subseq (symbol-name sym) n))))

(defmacro nlet (fn-name letargs &body body)
  (setf letargs (formalize-letargs letargs))
  `(labels ((,fn-name ,(mapcar #'car letargs)
              ,@body))
     (,fn-name ,@(mapcar #'cadr letargs))))

(defmacro nlet-acc (fn-name letargs  &body body)
  (with-gensyms (acc)
    `(let ((,acc '()))
       (flet ((accumulate (x) (push x ,acc)))
         (nlet ,fn-name ,letargs
           ,@body))
       (nreverse ,acc))))

(defun flatten (lst)
  (nlet-acc self ((x lst))
    (if (consp x)
	(progn (self (car x)) (self (cdr x)))
      (when x
	(accumulate x)))))