(in-package :pvme)

(defstruct symbol-table
  (table t :type hash-table))

(defun create-symbol-table ()
  (make-symbol-table :table (make-hash-table)))

#|
key:string => value:t
|#

(defun get-symbol-value (symbol-table key)
  (declare (symbol key))
  (with-slots (table) (the symbol-table symbol-table)
    (multiple-value-bind (value exists?) (gethash key table)
      (assert exists? () "The key ~s does not exist in symbol-table" key)
      value)))

(defun set-symbol-value (symbol-table key value)
  (declare (symbol key))
  (with-slots (table) (the symbol-table symbol-table)
    (setf (gethash key table) value)
    symbol-table))
