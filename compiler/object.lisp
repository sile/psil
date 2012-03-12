(in-package :pc)

(defconstant +INT_TAG+ 1)
(defconstant +CONS_TAG+ 2)
(defconstant +NIL_TAG+ 3)
(defconstant +STR_TAG+ 4)
(defconstant +CHAR_TAG+ 5)
(defconstant +VEC_TAG+ 6)
(defconstant +BOOL_TAG+ 7)

(defun int (n)
  `(,($ :int) ,(int-to-bytes n)))

(defun set-field (field value)
  `(,($ :d.dup) ,value ,field ,($ :d.rot) ,($ :m.set)))

(defun get-field (field)
  `(,($ :d.dup) ,field ,($ :d.rot) ,($ :m.get)))

(defun get-field2 (field)
  `(,field ,($ :d.swap) ,($ :m.get)))

(defun obj (size tag)
  `(,(int (1+ size)) ,($ :m.alloc)    ; allocate
    ,(set-field (int 0) (int tag))))  ; set tag

(defun from-int (n)
  `(,(obj 1 +INT_TAG+)
    ,(set-field (int 1) (int n))))

(defun from-char (c)
  `(,(obj 1 +CHAR_TAG+)
    ,(set-field (int 1) (int (char-code c)))))

(defun from-nil ()
  ;; TODO: symbol
  (obj 0 +NIL_TAG+))

(defun from-list (list)
  (if (null list)
      (from-nil)
    `(,(obj 2 +CONS_TAG+)
      ,(set-field (int 1) (from-object (car list)))
      ,(set-field (int 2) (from-object (cdr list))))))

(defun from-string (str &aux (len (length str)))
  `(,(obj (1+ len) +STR_TAG+)
    ,(set-field (int 1) (int len))
    ,(loop FOR c ACROSS str
           FOR i FROM 2
           COLLECT (set-field (int i) (from-char c)))))

(defun int= ()
  `(,($ :i.=) 
    ,(int 1)
    ,(obj 1 +BOOL_TAG+)
    ,($ :m.set)))
    
(defun char.= ()
  `(,(get-field2 1) ; c2 c1.value 
    ,($ :d.swap)    ; c1.value c2
    ,(get-field2 1) ; c1.value c2.value
    ,(int=)         ; bool
    ))

(defun 2dup ()
  `(,($ :d.dup) ,($ :d.rot) ,($ :d.dup)))

(let ((label 0))
  (defun next-label ()
    (incf label)))

(defun set-label (label)
  `(,($ :label) ,(int-to-bytes label)))

(defun @if (then else)
  (let ((then-label (next-label))
        (end-label (next-label)))
    `(,(int then-label)
      ,($ :jump-if)

      ;; else
      ,else
      ,(int end-label)
      ,($ :jump)
      
      ;; then
      ,(set-label then-label)
      ,then
      
      ,(set-label end-label))))

(defun @n-loop (body)
  (let ((start-label (next-label)))
    `(,body)))

(defun str.ref ()
  `(,($ :d.swap) ,($ :m.get)))

(defun str.=.impl (&aux (end-label (next-label)))
  `(,($ :d.dup) ,(str.len) ; s2 s1 length
    ,(@n-loop ; s2 s1  [r] i
      `(,(2dup)      ; s2 s1 s2 s1
        ,($ :r.copy) ; s2 s1 s2 s1 i
        ,(str.ref)   ; s2 s1 s2 s1[i]
        ,($ :d.swap) ; s2 s1 s1[i] s2
        ,($ :r.copy) ; s2 s1 s1[i] s2 i
        ,(str.ref)   ; s2 s1 s1[i] s2[i]
        ,(char.=)    ; s2 s1 bool
        
        ,(get-field2 (int 1))
        ,(@if '() `(,(from-boolean nil) ,(int end-label) ,($ :jump)))))
    ,(from-boolean t)
    ,(set-label end-label)))

(defun str.= ()
  `(,(2dup) ; s2 s1 s2 s1
    ,(str.len) ,($ :d.swap) ,(str.len) ,($ :i.=) ; s2 s1 native.bool
    ,(@if (str.=.impl) (from-boolean nil))))

(defun str.len ()
  (get-field2 (int 1)))

(defun from-vector (vec &aux (len (length vec)))
  `(,(obj (1+ len) +VEC_TAG+)
    ,(set-field (int 1) (int len))
    ,(loop FOR x ACROSS vec
           FOR i FROM 2
           COLLECT (set-field (int i) (from-object x)))))
  
(defun from-boolean (obj)
  `(,(obj 1 +BOOL_TAG+)
    ,(set-field (int 1) (int (if obj 1 0)))))

(defun from-symbol (obj)
  )

(defparameter *quote* nil)

(defun from-exp (obj)
  )

(defun from-object (obj)
  (etypecase obj
    (boolean (from-boolean obj))
    (number (from-int obj))
    (list 
     (if (or *quote* (eq (car obj) 'quote))
         (let ((*quote* t))
           (from-list obj))
       (from-exp obj)))
    (character (from-char obj))
    (string (from-string obj))
    (vector (from-vector obj))
    (symbol (from-symbol obj))
    ))
  
#|
整数
コンス
ニル
文字列
文字
配列
真偽値

シンボル

; ストリーム
; 構造体
|#
