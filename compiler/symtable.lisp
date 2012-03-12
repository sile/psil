(in-package :pc)

(defconstant +VAR_SYMTABLE+ 1)

(defun init-symtable ()
  `(,($ :variable) ,(int-to-bytes +VAR_SYMTABLE+)
    ,(from-nil) ,(int +VAR_SYMTABLE+) ,($ :setval)))

(defun get-val (var)
  `(,(int var) ,($ :getval)))

(defun set-val (var value)
  `(,value ,(int var) ,($ :setval)))

(defun sym.intern (name)
  )
