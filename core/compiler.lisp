(in-package :pbc-core)

;; 簡易コンパイラ
;; => パッケージを分けた方が良さそう

(defun flatten (x)
  (cond ((null x) x)
        ((atom x) (list x))
        (t (append (flatten (car x)) (flatten (cdr x))))))

(defun $$ (x)
  (cond ((null x)   x)
        ((arrayp x) ($$ (coerce x 'list)))
        ((atom x)   (list x))
        (t (append  ($$ (car x)) ($$ (cdr x))))))

(defun $ (&rest x)
  (apply #'append (mapcar #'$$ x)))

(defun int-to-bytes (n)
  (loop FOR i FROM 3 DOWNTO 0
        COLLECT (ldb (byte 8 (* i 8)) n)))

(defun short-to-bytes (n)
  (loop FOR i FROM 1 DOWNTO 0
        COLLECT (ldb (byte 8 (* i 8)) n)))

(defun @int (n)
  (declare ((signed-byte 32) n))
  ($ 1 (int-to-bytes n)))

(defun @string (str)
  (declare (simple-string str))
  (let ((o (sb-ext:string-to-octets str)))
    ($ 2 (int-to-bytes (length o)) o)))

(defun @char (ch)
  (declare (character ch))
  ($ 3 (int-to-bytes (char-code ch))))

(defun @symbol (symbol)
  (declare (symbol symbol))
  (let* ((name (symbol-name symbol))
         (octets (sb-ext:string-to-octets name)))
    ($ 4 (short-to-bytes (length octets)) octets)))

(defun @nil () ($ 5))
(defun @true () ($ 6))
(defun @false () ($ 7))

(defun val (sym)
  ($ (@symbol sym) 50))
  
(defun call (sym &rest args)
  ($ args (@symbol sym) 50 101))

(declaim (ftype (function (t) t) compile-bc))
(defun @list (list)
  (if (null list)
      (@nil)
    (call :$cons (compile-bc (car list)) (@list (cdr list)))))

(defun @if (condition then else)
  (let* ((then-bc (flatten (compile-bc then)))
         (else-bc (flatten ($ (compile-bc else) (@int (length then-bc)) 150))))
    ($ (compile-bc condition) (@int (length else-bc)) 151 else-bc then-bc)))

;; とりあえずマクロなしと、ローカル変数無しを仮定する
(defparameter *bindings* nil)
(defun make-fun-body (args body)
  (let ((*bindings* (loop FOR a IN (reverse args)
                          FOR i FROM 0
                          COLLECT (cons a i))))
    ;; returnがimplicit-prognを兼ねているかも
    (list 0 ($ (compile-bc (cons 'progn body)) 103))))

(defun @defun (name args body)
  (destructuring-bind (local-var-count body-bc) (make-fun-body args body)
    (let ((fn ($ 201 0 (length args) local-var-count (int-to-bytes (length body-bc)) body-bc)))
      ($ fn (@symbol name) 51))))

(defparameter *quote?* nil)

(defun compile-bc (exp)
  (etypecase exp
    (null   (@nil))
    (fixnum (@int exp))
    (string (@string exp))
    (character (@char exp))
    (symbol (if (assoc exp *bindings*)
                ($ 202 (cdr (assoc exp *bindings*)))
              (case exp
                (:true (@true))
                (:false (@false))
                (otherwise (if *quote?*
                               (@symbol exp)
                             ($ (@symbol exp) 50))))))
    (list 
     (if *quote?*
         (@list exp)
     (destructuring-bind (car . cdr) exp
       (case car
         (quote (let ((*quote?* t))
                  (assert (= (length cdr) 1) () "TODO")
                  (compile-bc (car cdr))))
         (if (destructuring-bind (condition then &optional else) cdr
               (@if condition then else)))

         ;; TODO:
         ;; let
         
         (progn (let ((last (car (last cdr)))
                      (butlast (butlast cdr)))
                  ($ (mapcar #'compile-bc butlast) 181 (length butlast) (compile-bc last))))

         ;; TODO: lambdaに一般化
         ;;       closureとかを除いて単純化するためにdefun
         (defun (destructuring-bind (name args &rest body) cdr
                  (@defun name args body)))
         (otherwise
          (apply #'call car (mapcar #'compile-bc cdr)))))))))

(defun compile-bytecode (exp &optional *quote?*)
  (flatten (compile-bc exp)))
