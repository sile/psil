(defpackage psil.evaluator
  (:use :common-lisp)
  (:shadow :common-lisp eval)
  (:export eval
           init-env))
(in-package :psil.evaluator)

(defun init-env ()
  )

(defun @sym (name)
  (list :symbol name))

(defun @exp (car cdr)
  (list :cons (list car cdr)))

(defun @cons (car cdr)
  (@exp car cdr))

(defun @nil ()
  (list :nil nil))

(defun @num (n)
  (list :number n))

(defun @mapc2 (fn list1 list2)
  (labels ((recur (xs ys)
             (unless (or (@nil-p xs) (@nil-p ys))
               (with-car-cdr (x xs) xs
                 (with-car-cdr (y ys) ys
                   (funcall fn x y)
                   (recur xs ys))))))
    (recur list1 list2)))

(defun @mapcar (fn list)
  (labels ((recur (xs)
             (unless (@nil-p xs)
               (with-car-cdr (x xs) xs
                 (cons (funcall fn x)
                       (recur xs))))))
    (recur list)))

(defstruct env
  (symbol-bindings (list '())))

(defun in-scope (env) (push '() (env-symbol-bindings env)))
(defun out-scope (env) (pop (env-symbol-bindings env)))
(defmacro with-scope ((env) &body body)
  (let ((e (gensym)))
    `(let ((,e ,env))
       (in-scope ,e)
       (unwind-protect
           (locally ,@body)
         (out-scope ,e)))))

(defparameter *global-env* (make-env))

(defun symbol-lookup (sym env)
  (labels ((recur (bindings-list)
             (if (null bindings-list)
                 (if (eq env *global-env*)
                     (values nil nil)
                   (symbol-lookup sym *global-env*))
               (destructuring-bind (bindings . rest) bindings-list
                 (if (null #1=(assoc sym bindings :test #'equal))
                     (recur rest)
                   (values (cdr #1#) t))))))
    (recur (env-symbol-bindings env))))

(defun bind-symbol (sym var env)
  (push `(,sym . ,var) (first (env-symbol-bindings env)))
  env)

(defun eval-symbol (symbol env)
  (multiple-value-bind (value exists?) (symbol-lookup (second symbol) env)
    (if exists?
        value
      (error "[PSIL] The variable ~@(~a~) is unbounded." symbol))))

(defun self-sym (name)
  `(,name . (:symbol ,name)))

(defun native-fun-sym (name fun)
  `(,name . (:native-function ,fun)))

(defun native.apply (fn args)
  (let ((real-args (@mapcar #'second args)))
    (apply fn real-args)))

(defun n.+ (args) (@num (native.apply #'+ args)))
(defun n.- (args) (@num (native.apply #'- args)))
(defun n.* (args) (@num (native.apply #'* args)))
(defun n./ (args) (@num (native.apply #'/ args)))
(defun n.= (args) (if (native.apply #'= args) (@sym "t") (@nil)))
(defun n./= (args) (if (native.apply #'/= args) (@sym "t") (@nil)))
(defun n.< (args) (if (native.apply #'< args) (@sym "t") (@nil)))
(defun n.> (args) (if (native.apply #'> args) (@sym "t") (@nil)))
(defun n.<= (args) (if (native.apply #'<= args) (@sym "t") (@nil)))
(defun n.>= (args) (if (native.apply #'>= args) (@sym "t") (@nil)))
(defun n.define (args) 
  (destructuring-bind (sym value . rest) (@mapcar #'identity args)
    (declare (ignore rest))
    (bind-symbol (second sym) value *global-env*)
    sym))
(defun n.print (args)
  (with-car-cdr (car cdr) args
    (declare (ignore cdr))
    (print car)))
(defun n.list (args)
  args)

(defparameter *system-symbols* '("nil" "t"
                                 "macro-lambda" 
                                 "lambda" "progn" "if"))

(defun predefined-symbols ()
  `(
    ("nil" . (:nil nil))
    ,(self-sym "t")
    ,(self-sym "lambda")
    ,(self-sym "progn")
    ,(self-sym "if")
    ,(self-sym "macro-lambda")
    ,(native-fun-sym "identity" #'identity)
    ,(native-fun-sym "+" #'n.+)
    ,(native-fun-sym "-" #'n.-)
    ,(native-fun-sym "/" #'n./)
    ,(native-fun-sym "*" #'n.*)
    ,(native-fun-sym "=" #'n.=)
    ,(native-fun-sym "/=" #'n./=)
    ,(native-fun-sym "<" #'n.<)
    ,(native-fun-sym ">" #'n.>)
    ,(native-fun-sym "<=" #'n.<=)
    ,(native-fun-sym ">=" #'n.>=)
    ,(native-fun-sym "define" #'n.define)
    ,(native-fun-sym "print" #'n.print)
    ,(native-fun-sym "list" #'n.list)  ; XXX: consで十分
    ))

(defun null-env ()
  (make-env :symbol-bindings (list (predefined-symbols))))
(defun copy-env (env)
  (make-env :symbol-bindings (copy-tree (env-symbol-bindings env))))

(defun exp-car-type (exp)
  (destructuring-bind (type value) exp
    (case type
      (:symbol (if (find value *system-symbols* :test #'string=)
                   :special-form
                 :symbol))
      (:function :function)
      (:macro :macro)
      (:native-function :native-function)
      (otherwise :other))))

(defun make-function (args body env)
  `(:function ,(list args (@exp (@sym "progn") body) (copy-env env))))

(defun make-macro-function (args body env)
  `(:macro ,(list args (@exp (@sym "progn") body) (copy-env env))))

(defmacro with-car-cdr ((car cdr) cons &body body)
  `(destructuring-bind (_ (,car ,cdr)) ,cons
     (declare (ignore _))
     ,@body))

(defun @nil-p (x) (eq :nil (first x)))

(defun eval-progn (exps env)
  (with-car-cdr (car cdr) exps
    (let ((val (eval car env)))
      (if (@nil-p cdr)
          val
        (eval-progn cdr env)))))

(defun eval-if (exps env)
  ;; TODO: assert
  (with-car-cdr (condition rest) exps
    (with-car-cdr (then-exp rest) rest
      (with-car-cdr (else-exp rest) rest
        (assert (@nil-p rest))
        (if (not (@nil-p (eval condition env)))
            (eval then-exp env)
          (eval else-exp env))))))
        
(defun eval-special-form (car cdr env &aux (symname (second car)))
  (cond ((string= symname "lambda")
         (with-car-cdr (args body) cdr
           (make-function args body env)))
        ((string= symname "macro-lambda")
         (with-car-cdr (args body) cdr
           (make-macro-function args body env)))
        ((string= symname "progn")
         (eval-progn cdr env))
        ((string= symname "if")
         (eval-if cdr env))
        (t (error "eval-special-form: ~a" car))))

(defmacro @dolist ((x xs) &body body)
  (let ((recur (gensym))
        (rest (gensym)))
    `(labels ((,recur (,rest)
                (unless (@nil-p ,rest)
                  (with-car-cdr (,x ,rest) ,rest
                    ,@body
                    (,recur ,rest)))))
       (,recur ,xs))))

(defun bind-args (vars args env)
  (@mapc2 (lambda (v a)
            (bind-symbol (second v) a env))
          vars args))
    
(defun eval-function (fun args _env)
  (declare (ignore _env))
  (destructuring-bind (type (vars body env)) fun
    (declare (ignore type))
    (with-scope (env) 
      (bind-args vars args env)
      (eval body env))))

(defun eval-args (args env)
  (if (@nil-p args)
      (@nil)
    (with-car-cdr (arg rest) args
      (@cons (eval arg env)
             (eval-args rest env)))))

(defun eval-expression (exp env)
  (destructuring-bind (type (car cdr)) exp
    (declare (ignore type))
    (let ((car-val (eval car env)))
      (ecase (exp-car-type car-val)
        (:special-form (eval-special-form car-val cdr env))
        (:function (eval-function car-val (eval-args cdr env) env))
        (:macro (eval (eval-function car-val cdr env) env))
        (:native-function (funcall (second car-val) (eval-args cdr env)))
        (:symbol (print :in) (eval-expression (@exp car-val cdr) env))
        (:other (error "[PSIL] ~a is can't placed in car of exp" car))))))

(defun eval (exp &optional (env (null-env)))
  (destructuring-bind (type value) exp
    (ecase type
      (:function exp)
      (:macro exp)
      (:native-function exp)

      (:cons (eval-expression exp env))
      (:symbol (eval-symbol exp env))
            
      (:quote value)

      (:string exp)
      (:array exp)
      (:number exp)
      (:nil exp)))) ; TODO: nilの特別扱いを廃止?
#|
- special-form
 - lambda
 - if
 - define
- 基本関数
- env
|#