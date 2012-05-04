(in-package :plcc)

(defmacro $ (&rest exps &aux (x (gensym)))
  `(flatten 
    (list ,@(loop FOR e IN exps
                  COLLECT `(let ((,x ,e))
                             (typecase ,x
                               (keyword (ins ,x))
                               (t ,x)))))))

(defun @int (exp) ($ :int (int-to-bytes exp)))
(defun @char (exp) ($ :char (int-to-bytes (char-code exp))))
(defun @true () ($ :true))
(defun @false () ($ :false))
(defun @nil () ($ :nil))
(defun @undef () ($ :undef))
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

(defun @fixjump (offset)
  ($ :fix-jump (short-to-bytes offset)))

(defun @fixjump-if (offset)
  ($ :fix-jump-if (short-to-bytes offset)))

(defun @quote (exps &aux (*quote* t))
  (destructuring-bind (exp) exps
    (compile-impl exp)))

(defun @if (exps)
  (destructuring-bind (cnd then &optional (else :|undef|)) exps
    (let* ((then~ (compile-impl then))
           (else~ ($ (compile-impl else) (@fixjump (length then~)))))
      ($ (compile-no-tail cnd) (@fixjump-if (length else~)) else~ then~))))

(defun @begin (exps)
  (if (null exps)
      (@undef)
    (let ((last (car (last exps)))
          (butlast (butlast exps)))
      ($ (mapcar #'compile-no-tail butlast) :dropn (length butlast) (compile-impl last)))))

(defun inspect-write-closed-vars (a b)
  (declare (ignore a b))
  '())

(defstruct bind 
  name
  index
  readonly) ;; XXX:name => writable-closedとかなんとか

(defparameter *bindings* '())
(defparameter *local-var-index* 0)

(defun local-bind (name readonly)
  (make-bind :name name
             :index (1- (incf *local-var-index*))
             :readonly readonly))

(defun find-local-bind (var)
  (find var *bindings* :key #'bind-name))

(defun @set!-nopush (var val &optional initial &aux (val~ (compile-no-tail val)))
  (a.if (find-local-bind var)
        (let ((op (cond ((bind-readonly it) :localset)
                        (initial            :local-mkref)
                        (t                  :local-refset))))
          ($ val~ op (bind-index it)))
    ($ val~ (@intern var) :symset)))

(defun @symvalue (var)
  (a.if (find-local-bind var)
        (if (bind-readonly it)
            ($ :localget (bind-index it))
          ($ :local-refget (bind-index it)))
    ($ (@intern var) :symget)))

(defun @lambda (exps)
  (destructuring-bind (args . body) exps
    (let* ((body `(:begin ,@body))
           (closing-vars '())
           (closing-var-indices '())
           (closed-vars '())
           (local-var-count 2)
           (*tail* t)
           (*local-var-index* 0)
           (*bindings* (append (loop FOR var IN args
                                     COLLECT (local-bind var (not (find var closed-vars))))
                               *bindings*)))
      (declare (ignore closing-vars))
      (let ((body~ (compile-impl body)))
        ($ (mapcar (lambda (i) ($ :localref i)) closing-var-indices)
           :lambda (length closed-vars) (length args)
           local-var-count (int-to-bytes (+ 2 (length body~))) body~ :show-stack :return)))))

(defun @let (exps)
  (destructuring-bind (bindings . body) exps
    (let* ((body `(:begin ,@body))
           (vars (mapcar #'car bindings))
           (closed-vars (inspect-write-closed-vars vars body))
           (*bindings* (loop FOR var IN vars
                             COLLECT (local-bind var (not (find var closed-vars))))))
      
      ($ (loop FOR (var val) IN bindings
               COLLECT (@set!-nopush var val t))
         (compile-impl body)))))

(defun @apply (fn args)
  ($ (mapcar #'compile-no-tail args) (compile-no-tail fn) 
     (if *tail* :tail-apply :apply)))
           
(defparameter *quote* nil)
(defparameter *tail* t)
(defun @compile-symbol (exp)
  (case exp
    (:|true| (@true))
    (:|false| (@false))
    (:|undef| (@undef))
    (otherwise (if *quote* 
                   (@intern exp) 
                 (@symvalue exp)))))

(defun @compile-list (exp)
  (destructuring-bind (car . cdr) exp
    (case car
      (:quote (@quote cdr))
      (:if    (@if cdr))
      (:begin (@begin cdr))
      (:lambda (@lambda cdr))
      (:define )
      (:set! )
      (:let (@let cdr))
      (otherwise (@apply car cdr)))))

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
