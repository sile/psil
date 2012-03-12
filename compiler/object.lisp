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
  `(,(obj len +STR_TAG+)
    ,(loop FOR c ACROSS str
           FOR i FROM 1
           COLLECT (set-field (int i) (from-char c)))))

(defun from-vector (vec)
  `(,(obj (length vec) +VEC_TAG+)
    ,(loop FOR x ACROSS vec
           FOR i FROM 1
           COLLECT (set-field (int i) (from-object x)))))
  
(defun from-boolean (obj)
  `(,(obj 1 +BOOL_TAG+)
    ,(set-field (int 1) (int (if obj 1 0)))))

(defun from-object (obj)
  (etypecase obj
    (boolean (from-boolean obj))
    (number (from-int obj))
    (list (from-list obj))
    (character (from-char obj))
    (string (from-string obj))
    (vector (from-vector obj))
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
