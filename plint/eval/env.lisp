(in-package :plint.eval)

(defstruct env
  (symbol-table (make-hash-table :test #'equal) :type hash-table)
  (quote nil :type boolean))

(defun enable-quote (env)
  (let ((new (copy-env env)))
    (setf (env-quote new) t)
    new))

(defun copy-hash-table (table)
  (let ((new (make-hash-table :test (hash-table-test table))))
    (maphash (lambda (k v) 
               (setf (gethash k new) v))
             table)
    new))

(defparameter *default-symbol-table* (make-hash-table :test #'equal))

(defun default-env ()
  (make-env :symbol-table (copy-hash-table *default-symbol-table*)))

(defun symbol-intern (name env)
  (with-slots (symbol-table) env
    (unless (gethash name symbol-table)
      (setf (gethash name symbol-table) (make-type.symbol :name name
                                                          :value :undef
                                                          :parent nil)))
    (values (gethash name symbol-table))))

(defun get-symbol (name env)
  (values (gethash name (env-symbol-table env))))

(defun add-special (name native-fn)
  (setf (gethash name *default-symbol-table*)
        (make-type.symbol :name name
                          :value (make-type.special :name name 
                                                    :native-fn native-fn))))

(defun env-deep-copy (env)
  (make-env :symbol-table (copy-hash-table (env-symbol-table env))))
