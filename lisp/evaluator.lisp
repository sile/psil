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

(defun register-native-fun (symbol fun arity)
  (setf (gethash symbol (env-symbols *env*))
        (make-fun :type :native :fn fun :arity arity)))

(defun init-env ()
  (setf *env* (make-env))

  (loop FOR (sym fun arity) IN `(("+" ,#'native+ 2)
                                 ("-" ,#'native- 2)
                                 ("*" ,#'native* 2)
                                 ("/" ,#'native/ 2))
        DO
        (register-native-fun sym fun arity))
  *env*)

(defparameter *env* (init-env))

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
  (multiple-value-bind (value exists?) (gethash symbol (env-symbols *env*))
    (if exists?
        value
      (error "[PSIL] The variable ~@(~a~) is unbound." symbol))))

(defun cons-to-args (cons)
  (if (eq :nil (first cons))
      nil
    (destructuring-bind (type (car cdr)) cons
      (cons (eval car)#|XXX|# (cons-to-args cdr)))))

(defun eval-native-apply (fn arity args)
  (assert (= (length args) arity))
  (apply fn args))

(defun eval-apply (fn args)
  (with-slots (type fn arity) fn
    (ecase type
      (:native (eval-native-apply fn arity (cons-to-args args))))))

(defun eval-expression (exp)
  (destructuring-bind (type (car cdr)) exp
    (declare (ignore type))
    (let ((fn (eval-symbol car)))
      (eval-apply fn cdr))))

(defun eval (exp)
  (destructuring-bind (type value) exp
    (ecase type
      (:string exp)
      (:cons (eval-expression exp))
      (:array exp)
      (:symbol (eval-symbol exp))
      (:quote value)
      (:number exp)
      (:nil exp))))
