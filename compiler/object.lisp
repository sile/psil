(in-package :pc)

(defconstant +INT_TAG+ 1)
(defconstant +CONS_TAG+ 2)
(defconstant +NIL_TAG+ 3)
(defconstant +STR_TAG+ 4)
(defconstant +CHAR_TAG+ 5)
(defconstant +VEC_TAG+ 6)
(defconstant +BOOL_TAG+ 7)
(defconstant +SYM_TAG+ 8)
(defconstant +FUN_TAG+ 9)

(defun int (n)
  `(,($ :int) ,(int-to-bytes n)))

(defun set-field (field value)
  `(,($ :d.dup) ,value ,field ,($ :d.rot) ,($ :m.set)))

;; value obj
(defun set-field3 (field)
  `(,field ,($ :d.swap) ; value field obj
    ,($ :m.set)))

;; value obj
(defun set-field2 (field)
  `(,($ :d.dup) ,($ :r.>)
    ,(set-field3 field)
    ,($ :r.<)))
       
(defun get-field (field)
  `(,($ :d.dup) ,field ,($ :d.rot) ,($ :m.ref)))

(defun get-field2 (field)
  `(,field ,($ :d.swap) ,($ :m.ref)))

(defun obj (size tag)
  `(,(int (1+ size)) ,($ :m.alloc)    ; allocate
    ,(set-field (int 0) (int tag))))  ; set tag

(defun int.val ()
  (get-field2 (int 1)))

(defun to-int ()
  `(,(obj 1 +INT_TAG+)
    ,(set-field2 (int 1))))

(defun from-int (n)
  `(,(obj 1 +INT_TAG+)
    ,(set-field (int 1) (int n))))

(defun from-char (c)
  `(,(obj 1 +CHAR_TAG+)
    ,(set-field (int 1) (int (char-code c)))))

(defun from-nil ()
  ;; TODO: symbol
  (obj 0 +NIL_TAG+))

;; car cdr
(defun @cons ()
  `(,(obj 2 +CONS_TAG+)
    ,(set-field2 (int 2))
    ,(set-field2 (int 1))))

(defun bool.val ()
  (get-field2 (int 1)))

(defun cons.car ()
  (get-field2 (int 1)))

(defun cons.cdr ()
  (get-field2 (int 2)))

;; val cons
(defun cons.cdr.set ()
  (set-field3 (int 2)))

(defun @nil? ()
  `(,(get-field2 (int 0))
    ,(int +NIL_TAG+)
    ,($ :i.=)))
    
(defun @list-loop-clean ()
  `(,($ :r.<) ,($ :r.<) ,($ :d.drop) ,($ :d.drop)))

(defun @list-loop (body)
  (let ((start-label (next-label))
        (end-label (next-label)))
    `(,(set-label start-label)
      ,($ :d.dup) ; list list
      ,(@nil?) ; list native.bool
      ,(int end-label)
      ,($ :jump-if)
      
      ,($ :d.dup) ; list list
      ,($ :r.>)   ; list  [r] list
      ,(cons.car) ; car   [r] list
      ,($ :r.>)   ; [r] list car
      
      ,body

      ,($ :r.<) ,($ :d.drop) ,($ :r.<) ; list
      ,(cons.cdr)  ; list
      ,(int start-label)
      ,($ :jump)
      
      ,(set-label end-label)
      ,($ :d.drop))))

;; key:string list
(defun @assoc (&aux (end-label (next-label)))
  `(,(@list-loop 
      `(,($ :d.dup)  ; key key
        ,($ :r.copy) ; key key cons
        ,(cons.car)  ; key key car
        ,(str.=)     ; key bool
        ,(bool.val)  ; key native.bool
        ,(@if `(,($ :r.copy) ,(@list-loop-clean) ,(int end-label) ,($ :jump))
              '())))
    ,(from-nil)
    ,(set-label end-label)
    ,($ :d.swap) ,($ :d.drop)))

;; car cdr list => cons list
(defun @acons ()
  `(,($ :d.rot) ,($ :d.rot) ; list car cdr
    ,(@cons) ; list cons
    ,($ :d.dup) ,($ :d.rot) ; cons cons list
    ,(@cons))) ; cons list

(defun from-list (list)
  (if (null list)
      (from-nil)
    `(,(from-object (car list))
      ,(if (null (cdr list)) (from-nil) (from-object (cdr list)))
      ,(@cons))))

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
    ,($ :d.dup)
    ,($ :r.>)
    ,($ :m.set)
    ,($ :r.<)))
    
(defun char.= ()
  `(,(get-field2 (int 1)) ; c2 c1.value 
    ,($ :d.swap)    ; c1.value c2
    ,(get-field2 (int 1)) ; c1.value c2.value

    ,(int=)         ; bool
    ))

(defun 2dup ()
  `(,($ :d.dup) ,($ :d.rot) ,($ :d.dup) ,($ :d.rot)))

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

(defun loop-clean ()
  `(,($ :r.<) ,($ :r.<) ,($ :d.drop) ,($ :d.drop)))

(defun @n-loop (body)
  (let ((start-label (next-label))
        (end-label (next-label)))
    `(,($ :d.dup)
      ,($ :r.>)
      
      ,(int 0) ; limit i  [r] limit i
      ,($ :d.dup)
      ,($ :r.>)
      
      ,(set-label start-label)
      ,($ :i.>=)
      ,(int end-label)
      ,($ :jump-if)
      ,body
      ,($ :r.<) ,(int 1) ,($ :i.add)
      ,($ :r.copy) 
      ,($ :d.swap)
      ,($ :d.dup) ,($ :r.>)
      
      ,(int start-label)
      ,($ :jump)
      ,(set-label end-label)
      ,(loop-clean)
      )))

(defun debug.print-a ()
  `(,(int 97) ,($ :c.print)
    ,(int 10) ,($ :c.print)))

(defun str.ref ()
  `(,(int 2) ,($ :i.add) ,($ :d.swap) ,($ :m.ref)))

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

        ,(@if '() 
              `(,(loop-clean) ,(from-boolean nil) ,(int end-label) ,($ :jump)))))
    ,(from-boolean t)
    ,(set-label end-label)
    ))

(defun str.= ()
  `(,(2dup) ; s2 s1 s2 s1
    ,(str.len) ,($ :d.swap) ,(str.len) ,($ :i.=) ; s2 s1 native.bool
    ,(@if (str.=.impl) 
          (from-boolean nil))
    ,($ :d.rot) ,($ :d.rot) ,($ :d.drop) ,($ :d.drop)))

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
  (let ((name (symbol-name obj)))
    `(,(obj 1 +SYM_TAG+)
      ,(from-string name) ; obj str
      ,(sym.intern)   ; obj sym
      ,($ :d.swap)    ; sym obj
      ,(set-field2 (int 1)) ; obj
      )))

(defun @fun (body)
  (let ((start (next-label))
        (end (next-label)))
    `(,(obj 1 +FUN_TAG+)
      ,(set-field (int 1) (int start))
      
      ,(int end)
      ,($ :jump)
      ,(set-label start)
      ,body
      ,($ :return)
      ,(set-label end))))

;; fun
(defun fun.label ()
  (get-field2 (int 1)))

(defun sym.val ()
  `(,(get-field2 (int 1)) ; cons
    ,(cons.cdr)))

;; val sym
(defun sym.val.set ()
  `(,(get-field2 (int 1)) ; val con
    ,(cons.cdr.set)))     ; 

(defun @debug (var env)
  (let ((val (gethash var (pvm::env-heap env))))
    (ecase (aref val 0)
      (1 `(int ,(aref val 1)))
      (2 (cons (@debug (aref val 1) env) (@debug (aref val 2) env)))
      (3 nil)
      (4 `(str ,(map 'vector (lambda (x) (@debug x env)) (subseq val 2 (+ 2 (aref val 1))))))
      (5 `(char ,(aref val 1)))
      (6 `(vec ,(map 'vector (lambda (x) (@debug x env)) (subseq val 2 (+ 2 (aref val 1))))))
      (7 `(bool ,(aref val 1)))
      (8 `(sym ,(aref val 1)))
      )))

(defparameter *quote?* nil)

(defun @funcall ()
  `(,($ :r.>-n) ; fun num [r] ... 
    ,($ :r.>)
    ,(fun.label) ; label [r] num ...
    ,($ :call)
    ,($ :r.<)   ; num 
    ,($ :r.drop-n)
    ))

(defun @reg-fun (sym fun)
  `(,fun
    ,sym
    ,(sym.val.set)))

(defun lvar (index)
  `(,(int (+ index 2)) ,($ :r.ref)))

(defun fun_add ()
  (@fun `(,(lvar 0) ,(int.val)
          ,(lvar 1) ,(int.val)
          ,($ :i.add) ,(to-int))))

(defun eval-args (args &aux (len (length args)))
  `(,(int len)
    ,(loop FOR x IN args COLLECT (from-object x))
    ,(int len)))
  
;; XXX: 簡易 (car部の評価なし)
(defun from-exp (obj)
  (destructuring-bind (fun-name . args) obj
    `(,(from-symbol fun-name) ,(sym.val) ; fun (assumed)
      ,(eval-args args)                  ; args
      ,(@funcall))))

(defun from-object (obj)
  (etypecase obj
    (boolean (from-boolean obj))
    (number (from-int obj))
    (list 
     (cond ((eq (car obj) 'quote)
            (let ((*quote?* t))            
              (from-list (second obj))))
           (*quote?*
            (from-list obj))
           (t
            (from-exp obj))))
    (character (from-char obj))
    (string (from-string obj))
    (vector (from-vector obj))
    (symbol (from-symbol obj))
    ))

(defun init-built-in-fun ()
  `(
    ,(@reg-fun (from-symbol 'add) (fun_add))
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
