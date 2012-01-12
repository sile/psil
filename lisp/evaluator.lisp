(defpackage psil.evaluator
  (:use :common-lisp)
  (:shadow :common-lisp eval)
  (:export eval
           init-env))
(in-package :psil.evaluator)

(defun @sym (name)
  (list :symbol name))

(defun @exp (car cdr)
  (list :cons (list car cdr)))
#|
(eval-when (:load-toplevel :compile-toplevel)
 (defun native+ (x y)
  (make-number (+ (number-value x) (number-value y))))

 (defun native- (x y)
  (make-number (- (number-value x) (number-value y))))

 (defun native* (x y)
  (make-number (* (number-value x) (number-value y))))

 (defun native/ (x y)
  (make-number (/ (number-value x) (number-value y)))))

(defstruct env
  (symbols (make-hash-table :test 'equal)))

(defstruct fun
  type
  fn
  arity)

(defstruct lmd
  args
  body)

(defun special-lambda (a &aux (args (cons-to-args (car a))) (body (cdr a)))
  (assert (every (lambda (a)
                   (eq (car a) :symbol)) args))
  (make-fun :type :lambda
            :fn (make-lmd :args args :body body)))

(defun register-native-fun (symbol fun arity)
  (setf (gethash symbol (env-symbols *env*))
        (make-fun :type :native :fn fun :arity arity)))

(defun register-special-fun (symbol fun arity)
  (setf (gethash symbol (env-symbols *env*))
        (make-fun :type :special :fn fun :arity arity)))

(defun init-env ()
  (setf *env* (make-env))

  (loop FOR (sym fun arity) IN `(("+" ,#'native+ 2)
                                 ("-" ,#'native- 2)
                                 ("*" ,#'native* 2)
                                 ("/" ,#'native/ 2))
        DO
        (register-native-fun sym fun arity))

  (loop FOR (sym fun arity) IN `(("lambda" ,#'special-lambda nil))
        DO
        (register-special-fun sym fun arity))

  *env*)

(defparameter *env* (init-env))
(defparameter *local-env* (make-env))

(defun exp-nil? (exp)
  (eq (car exp) :nil))

(defun number-value (x)
  (assert (eq :number (first x)) () "x=~a" x)
  (second x))

(defun make-number (x)
  (list :number x))

(defun eval-symbol (exp &aux (symbol (second exp)))
  (multiple-value-bind (value) (or (gethash symbol (env-symbols *local-env*))
                                           (gethash symbol (env-symbols *env*)))
                                           
    (if value
        value
      (error "[PSIL] The variable ~@(~a~) is unbound." symbol))))

(defun cons-to-args (cons)
  (if (eq :nil (first cons))
      nil
    (destructuring-bind (type (car cdr)) cons
      (declare (ignore type))
      (cons car (cons-to-args cdr)))))

(defun eval-native-apply (fn arity args)
  (assert (= (length args) arity))
  (apply fn args))

(defun eval-special (fn args)
  (funcall fn args))

(defun eval-lambda (lmd args)
  (let ((*local-env* (make-env)))
    (loop FOR var IN (lmd-args lmd)
          FOR val IN args
      DO
      (setf (gethash (second var)#|xxx|# (env-symbols *local-env*)) val))
    (eval (car (lmd-body lmd)))))

(defun eval-apply (fn args)
  (with-slots (type fn arity) fn
    (let ((args (cons-to-args args)))
      (ecase type
        (:lambda (eval-lambda fn args))
        (:special (eval-special fn args))
        (:native (eval-native-apply fn arity (mapcar #'eval args)))))))

(defun eval-expression (exp)
  (destructuring-bind (type (car cdr)) exp
    (declare (ignore type))
    (let ((fn (ecase (first car)
                (:symbol (eval-symbol car))
                (:cons (eval car)))))
      (eval-apply fn cdr))))

#+C
(defun eval (exp)
  (destructuring-bind (type value) exp
    (ecase type
      (:function exp)
      (:string exp)
      (:cons (eval-expression exp))
      (:array exp)
      (:symbol (eval-symbol exp))
      (:quote value)
      (:number exp)
      (:nil exp))))

|#

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

(defun symbol-lookup (sym env)
  (labels ((recur (bindings-list)
             (if (null bindings-list)
                 (values nil nil)
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

(defparameter *system-symbols* '("nil" "t"
                                 "lambda" "progn" "if"))

(defun self-sym (name)
  `(,name . (:symbol ,name)))

(defun predefined-symbols ()
  `(
    ("nil" . (:nil nil))
    ,(self-sym "t")
    ,(self-sym "lambda")
    ,(self-sym "progn")
    ,(self-sym "if")
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
      (otherwise :other))))

(defun make-function (args body env)
  (list :function
        args (@exp (@sym "progn") body) (copy-env env)))

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
        ((string= symname "progn")
         (eval-progn cdr env))
        ((string= symname "if")
         (eval-if cdr env))
        (t (error "eval-special-form: ~a" car))))

(defun eval-expression (exp env)
  (destructuring-bind (type (car cdr)) exp
    (declare (ignore type))
    (let ((car-val (eval car env)))
      (ecase (exp-car-type car-val)
        (:special-form (eval-special-form car-val cdr env))
        (:function)
        (:symbol 'todo) ; re-evaluate
        (:other (error "[PSIL] ~a is can't placed in car of exp" car))))))

(defun eval (exp &optional (env (null-env)))
  (destructuring-bind (type value) exp
    (ecase type
      (:function 'todo)
      
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