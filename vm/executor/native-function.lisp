(in-package :pvme)

(defmacro defnative (name args &body body)
  `(defparameter ,name
     (make-fun :arity ,(length args)
               :body (lambda ()
                       (let ,(loop FOR a IN args 
                                   FOR i FROM (1- (length args)) DOWNTO 0
                                   COLLECT `(,a (local-ref +stack+ ,i)))
                         (spush +stack+ (locally ,@body)))))))

(defnative $add (x y) (+ x y))
(defnative $sub (x y) (- x y))
(defnative $mul (x y) (* x y))
(defnative $div (x y) (/ x y))
(defnative $mod (x y) (mod x y))

(defnative $make-array (size) (make-array size))
(defnative $ary-ref (ary i) (aref ary i))
(defnative $ary-set (ary i value) (setf (aref ary i) value))
(defnative $ary-len (ary) (length ary))

(defnative $cons (car cdr) (cons car cdr))
(defnative $car (cons) (car cons))
(defnative $cdr (cons) (cdr cons))

(defnative $load-bytecode-file (path) (load-bc path))

(defparameter *natives*
  `(
    (:$+ ,$add)
    (:$- ,$sub)
    (:$* ,$mul)
    (:$/ ,$div)
    (:$mod ,$mod)

    (:$make-array ,$make-array)
    (:$ary-ref ,$ary-ref)
    (:$ary-set ,$ary-set)
    (:$ary-len ,$ary-len)

    (:$cons ,$cons)
    (:$car ,$car)
    (:$cdr ,$cdr)

    (:$load-bytecode-file ,$load-bytecode-file)
    ))

;;;;;
(defun sym (sym)
  (let ((name (symbol-name sym)))
    `(0 4 0 ,(length name) ,@(map 'list #'char-code name))))

(defun call (sym)
  `(,@(sym sym) 0 50 0 101))
