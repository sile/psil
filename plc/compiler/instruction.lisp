(in-package :plcc)

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

               (:symget 50)
               (:symset 51)
               (:constget 52)
               
               (:apply 101)
               (:tail-apply 102)
               (:return 103)
               (:conti 104)
               (:nuate 105)
               (:recur-tal-aply 106)
               
               (:jump 150)
               (:jump-if 151)
               (:fix-jump 152)
               (:fix-jump-if 153)

               (:drop 180)
               (:dropn 181)

               (:lambda 201)
               (:localget 202)
               (:localset 203)
               (:local-mkref 204)
               (:local-refget 205)
               (:local-refset 206)
               (:local-toref 207)

               (:print 250))))
    (loop FOR (name code) IN ins
          DO (setf (gethash name table) code))
    table))

(defun ins (name)
  (let ((code (gethash name *instructions*)))
    (assert code () "unknown instruction: ~s" name)
    code))