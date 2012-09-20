(in-package :plint.eval)

(defun special-begin (args-ast env)
  (if (null args-ast)
      (make-type.undef)
    (let ((last (car (last args-ast)))
          (butlast (butlast args-ast)))
      (dolist (exp butlast)
        (eval-ast exp env))
      (eval-ast last env))))

(defun special-if (args-ast env)
  (destructuring-bind (cond then &optional else) args-ast
    (let ((rlt (eval-ast cond env)))
      (if (or (not (typep rlt 'type.boolean))
              (not (null (type.boolean-value rlt))))
          (eval-ast then env)
        (if else
            (eval-ast else env)
          (make-type.undef))))))

(defun special-lambda (args-ast env)
  (destructuring-bind (params . body) args-ast
    (let ((params (mapcar (lambda (p) (eval-ast p (enable-quote env))) (cadr params))))
      (assert (every #'type.symbol-p params) () "parameters must be a symbol")
      (make-type.lambda :params params
                        :body `(:list ((:symbol "begin") ,@body)) ; implicit begin
                        :env (env-deep-copy env)))))

(defun special-_define_ (args-ast env)
  (destructuring-bind (var val) args-ast
    (let ((sym (eval-ast var (enable-quote env))))
      (declare (type.symbol sym))
      (setf (type.symbol-value sym) (eval-ast val env))))
  (make-type.undef))

;;
(add-special "begin" #'special-begin)
(add-special "if" #'special-if)
(add-special "lambda" #'special-lambda)
(add-special "_define_" #'special-_define_)
