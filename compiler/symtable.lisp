(in-package :pc)

(defconstant +VAR_SYMTABLE+ 1)

(defun init-symtable ()
  `(,($ :variable) ,(int-to-bytes +VAR_SYMTABLE+)
    ,(from-nil) ,(int +VAR_SYMTABLE+) ,($ :setval)

    ;;
    ,(_sym.intern)))

(defun get-val (var)
  `(,(int var) ,($ :getval)))

;; value
(defun set-val (var)
  `(,(int var) ,($ :setval)))

;; name
(let ((sym-label (next-label))
      (sym-end-label (next-label)))
  (defun sym.intern ()
    `(,(int sym-label)
      ,($ :call)))
  
  (defun _sym.intern ()
    `(,(int sym-end-label)
      ,($ :jump)
      ,(set-label sym-label)
      ,($ :d.dup) ,(get-val +VAR_SYMTABLE+)  ; name name list
      ,(@assoc) ; name cons|nil
      ,($ :d.dup) ; name cons|nil cons|nil
      ,(@nil?)    ; name cons|nil native.bool
      ,(@if `(,($ :d.drop) ; name 
              ,(from-nil)  ; name value
              ,(get-val +VAR_SYMTABLE+) ; name value list
              ,(@acons)   ; cons list
              ,(set-val +VAR_SYMTABLE+)) ; cons
           `(,($ :d.swap) ,($ :d.drop)))
      ,($ :return)
      ,(set-label sym-end-label))))
