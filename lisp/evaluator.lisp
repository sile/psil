(defpackage psil.evaluator
  (:use :common-lisp)
  (:shadow :common-lisp eval)
  (:export eval
           init-env))
(in-package :psil.evaluator)

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

(defun native+ (x y)
  (make-number (+ (number-value x) (number-value y))))

(defun native- (x y)
  (make-number (- (number-value x) (number-value y))))

(defun native* (x y)
  (make-number (* (number-value x) (number-value y))))

(defun native/ (x y)
  (make-number (/ (number-value x) (number-value y))))

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
