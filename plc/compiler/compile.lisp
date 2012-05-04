(in-package :plcc)

(defmacro $ (&rest exps)
  `(flatten 
    (list ,@(loop FOR e IN exps
                  COLLECT (typecase e
                            (keyword `(ins ,e))
                            (t e))))))

(defun @int (exp) ($ :int (int-to-bytes exp)))
(defun @char (exp) ($ :char (int-to-bytes (char-code exp))))
(defun @true () ($ :true))
(defun @false () ($ :false))
(defun @nil () ($ :nil))
(defun @string (exp &aux (o (sb-ext:string-to-octets exp)))
  ($ :string (int-to-bytes (length o)) (coerce o 'list)))

#+C
(defun @symbol (exp &aux (o (sb-ext:string-to-octets exp)))
  ($ :symbol (short-to-bytes (length o)) (coerce o 'list))) ;; => to const


(defparameter *constants* (make-hash-table))
(defun @intern (exp)
  (unless #1=(gethash exp *constants*)
    (setf #1# (hash-table-count *constants*)))
  ($ :constget (short-to-bytes #1#)))

(defun @symvalue (exp)
  ($ (@intern exp) :symget))

(defun @fixjump (offset)
  ($ :fix-jump (short-to-bytes offset)))

(defun @fixjump-if (offset)
  ($ :fix-jump-if (short-to-bytes offset)))

(defun @quote (exps &aux (*quote* t))
  (destructuring-bind (exp) exps
    (compile-impl exp)))

(defun @if (exps)
  (destructuring-bind (cnd then &optional else) exps
    (let* ((then~ (compile-impl then))
           (else~ ($ (compile-impl else) (@fixjump (length then~)))))
      ($ (compile-no-tail cnd) (@fixjump-if (length else~)) else~ then~))))

(defparameter *quote* nil)
(defparameter *tail* t)
(defun @compile-symbol (exp)
  (case exp
    (:|true| (@true))
    (:|false| (@false))
    (otherwise (if *quote* 
                   (@intern exp) 
                 (@symvalue exp)))))

(defun @compile-list (exp)
  (destructuring-bind (car . cdr) exp
    (ecase car
      (:quote (@quote cdr))
      (:if    (@if cdr))
      )))

(defun compile-impl (exp)
  (etypecase exp
    (null      (@nil))
    (integer   (@int exp))
    (string    (@string exp))
    (character (@char exp))
    (symbol    (@compile-symbol exp))
    (list      (@compile-list exp))))

(defun compile-no-tail (exp &aux (*tail* nil))
  (compile-impl exp))
