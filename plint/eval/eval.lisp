(in-package :plint.eval)

(defun eval-number (value env)
  (declare (ignore env))
  (make-type.number :value value))

(defun eval-string (value env)
  (declare (ignore env))
  (make-type.string :value value))

(defun eval-character (value env)
  (declare (ignore env))
  (make-type.character :value value))

(defun eval-boolean (value env)
  (declare (ignore env))
  (make-type.boolean :value value))

(defun get-symbol-value (symbol-name env)
  (let ((sym (get-symbol symbol-name env)))
    (when (null sym)
      (error "uninterned symbol: ~a" symbol-name))
    (when (eq (type.symbol-value sym) :undef)
      (error "undefined symbol: ~a" symbol-name))
    (type.symbol-value sym)))

(defun eval-symbol (value env)
  (if (env-quote env)
      (symbol-intern value env)
    (get-symbol-value value env)))

(defun eval-list (value env)
  (labels ((recur (list)
             (when list
               (make-type.cons :car (eval-ast (car list) env)
                               :cdr (recur (cdr list))))))
    (recur value)))

(defun eval-special (fn args env)
  (with-slots (native-fn) fn
    (funcall native-fn args env)))

(defun bind-params (params args env &aux (new-env (copy-env env)))
  ;; TODO: &optional, &key, &rest
  (assert (= (length params) (length args)) () "bind params failed")
  (loop FOR p IN params
        FOR a IN args
        FOR s = (symbol-intern (type.symbol-name p) new-env)
    DO
    (setf (type.symbol-value s) a))
  new-env)

(defun eval-apply (fn args env)
  (declare (ignore env))
  (with-slots (params body (closed-env env)) fn
    (let ((new-env (bind-params params args closed-env)))
      (eval-ast body new-env))))

(defun eval-macro (fn args env)
  (declare (ignore fn args env))
  )  

(defun eval-expression (value env)
  (destructuring-bind (fn-exp . args) value
    (let ((fn (eval-ast fn-exp env)))
      (etypecase fn
        (type.special (eval-special fn args env))
        (type.macro  (eval-ast (eval-macro fn (mapcar (lambda (a) (eval-ast a (enable-quote env))) args) env)
                               env))
        (type.lambda (eval-apply fn (mapcar (lambda (a) (eval-ast a env)) args) env))))))

(defun eval-ast (ast &optional (env (default-env)))
  (destructuring-bind (tag value) ast
    (ecase tag
      (:number (eval-number value env))
      (:string (eval-string value env))
      (:character (eval-character value env))
      (:boolean (eval-boolean value env))
      (:symbol (eval-symbol value env))
      (:list (if (env-quote env)
                 (eval-list value env)
               (eval-expression value env)))
      (:quote (eval-ast value (enable-quote env))))))
