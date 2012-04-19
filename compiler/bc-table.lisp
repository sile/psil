(in-package :plc)

(defparameter *instructions*
  (let ((table (make-hash-table :test 'eq))
        (ins '((:int 1)
               (:string 2)
               (:char 3)
               (:symbol 4)
               (:nil 5)
               (:true 6)
               (:false 7)
               (:list 8)

               (:symref 50)
               (:symset 51)
               
               (:apply 101)
               (:return 103)
               (:conti 104)
               (:nuate 105)
               
               (:jump 150)
               (:jump-if 151)

               (:drop 180)
               (:dropn 181)

               (:lambda 201)
               (:localref 202)
               (:localset 203)
               (:mkref 204)
               (:refref 205)
               (:refset 206)

               (:print 250))))
    (loop FOR (name code) IN ins
          DO (setf (gethash name table) code))
    table))

(defun i (name)
  (let ((code (gethash name *instructions*)))
    (assert code () "unknown instruction: ~s" name)
    code))

