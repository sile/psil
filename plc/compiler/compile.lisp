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

(defparameter *toplevel* t)

(defun @begin (exps)
  (if (null exps)
      (@undef)
    (labels ((recur (exp rest)
               (cond ((and (not *toplevel*) (consp exp) (eq :define (car exp)))
                      (@inner-define (cdr exp) rest))
                     (rest
                      ($ (compile-no-tail exp) :drop (recur (car rest) (cdr rest))))
                     (t
                      (compile-impl exp)))))
      (recur (car exps) (cdr exps)))))

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

(defun @set!-nopush (var val &optional initial)
  (a.if (find-local-bind var)
        (let ((op (cond ((bind-readonly it) :localset)
                        (initial            :local-mkref)
                        (t                  :local-refset))))
          ($ val op (bind-index it)))
    ($ val (@intern var) :symset)))

(defun @symvalue (var)
  (a.if (find-local-bind var)
        (if (bind-readonly it)
            ($ :localget (bind-index it))
          ($ :local-refget (bind-index it)))
    ($ (@intern var) :symget)))

(defun adjust-args (args)
  (loop FOR a IN args
        FOR v = (find-local-bind a)
        UNLESS (bind-readonly v)
        COLLECT ($ :local-toref (bind-index v))))

(defun normalize-args (args)
  (labels ((recur (cons n acc)
             (typecase cons
               (null (values (nreverse acc) n nil))
               (cons (recur (cdr cons) (1+ n) (cons (car cons) acc)))
               (t    (values (nreverse `(,cons ,@acc)) (1+ n) t)))))
    (recur args 0 '())))

(defun @lambda (exps &optional toplevel &aux (binded-vars (mapcar #'bind-name *bindings*)))
  (destructuring-bind (args . body) exps
    (multiple-value-bind (args arity vararg) (normalize-args args)
      (multiple-value-bind (free-vars mutable-free-vars) (@inspect `(:begin ,@body))
        (let* ((body `(:begin ,@body))
               (closing-vars (intersection free-vars 
                                           (set-difference binded-vars args)))
               (closing-var-indices (mapcar (lambda (v) (bind-index (find-local-bind v)))
                                            closing-vars))
               (closed-args (intersection mutable-free-vars args)) ; XXX: name 
               (*toplevel* toplevel)
               (*tail* t)
               (*local-var-index* 0)
               (*bindings* (append (loop FOR var IN args
                                         COLLECT (local-bind var (not (find var closed-args))))
                                   (loop FOR var IN closing-vars
                                         COLLECT (local-bind var (bind-readonly (find-local-bind var))))
                                   *bindings*)))
          (let ((body~ ($ (adjust-args args)
                          (compile-impl body)))
                (local-var-count (- *local-var-index* (length args) (length closing-vars))))
            ($ (mapcar (lambda (i) ($ :localget i)) closing-var-indices)
               :lambda (length closing-vars) arity
               local-var-count (if vararg 1 0)
               (int-to-bytes (1+ (length body~))) body~ :return)))))))

(defun @let (exps)
  (destructuring-bind (bindings . body) exps
    (let* ((body `(:begin ,@body))
           (vars (mapcar #'car bindings))
           (closed-vars (intersection (nth-value 1 (@inspect body)) vars))
           (*toplevel* nil)
           (old-bindings *bindings*)
           (*bindings* (append (loop FOR var IN vars
                                     COLLECT (local-bind var (not (find var closed-vars))))
                               *bindings*)))
      ($ (loop FOR (var val) IN bindings
               FOR val~ = (let ((*bindings* old-bindings)) 
                            (compile-no-tail val))
               COLLECT (@set!-nopush var val~ t))
         (compile-impl body)))))

(defun @apply (fn args)
  ($ (mapcar #'compile-no-tail args) (compile-no-tail fn) 
     (if *tail* :tail-apply :apply)
     (length args)))

(defun @list-apply (exps)
  (destructuring-bind (fn args) exps
    ($ (compile-no-tail args) (compile-no-tail fn) :list-apply)))
           
(defun @set! (exps)
  (destructuring-bind (var val) exps
    ($ (@set!-nopush var (compile-no-tail val)) (@undef))))

(defun @toplevel-define (exps)
  (destructuring-bind (var val) exps
    ($ (compile-no-tail val) (@intern var) :symset :undef)))

(defun @inner-define (exps subsequent-exps)
  (destructuring-bind (var val) exps
    (compile-impl `(:let ((,var ,val)) ,@subsequent-exps))))

(defun @list-impl (exps)
  (if (atom exps)
      `(:quote ,exps)
    `(:cons (:quote ,(car exps)) ,(@list-impl (cdr exps)))))

(defun @list (exps)
  (let ((*quote* nil))
    (compile-impl (@list-impl exps)))
  #+C
  ($ (mapcar #'compile-impl exps) :list (int-to-bytes (length exps))))

(defun @case-expand (exps)
  (destructuring-bind (exp . clauses) exps
    (labels ((recur (clauses)
               (if (null clauses)
                   :|undef|
                 (destructuring-bind ((data . exps) . rest) clauses 
                   `(:if (:or (:eqv? (:quote :else) (:quote ,data))
                             (:memv :case-tmp-x (:quote ,data)))
                        (:begin ,@exps)
                      ,(recur rest))))))
      `(:let ((:case-tmp-x ,exp))
             ,(recur clauses)))))

(defun @case (exps)
  (compile-impl (@case-expand exps)))

(defun @or-expand (exps)
  (if (null exps)
      :|false|
    (destructuring-bind (exp . rest) exps
      `(:let ((:or-tmp-x ,exp))
         (:if :or-tmp-x :or-tmp-x ,(@or-expand rest))))))

(defun @or (exps)
  (compile-impl (@or-expand exps)))

(defun @and-expand (exps)
  (if (null exps)
      :|true|
    (destructuring-bind (exp . rest) exps
      `(:let ((:and-tmp-x ,exp))
         (:if :and-tmp-x ,(if rest (@or-expand rest) :and-tmp-x) :|false|)))))

(defun @let*-expand (cdr)
  (destructuring-bind (bindings . body) cdr
    (labels ((recur (bindings)
               (if (null bindings)
                   `(:begin ,@body)
                 (destructuring-bind (binding . rest) bindings
                   `(:let (,binding)
                      ,(recur rest))))))
      (recur bindings))))

(defparameter *quote* nil)
(defparameter *tail* t)
(defun @compile-symbol (exp)
  (case exp
    (:|true| (@true))
    (:|false| (@false))
    (:|undef| (@undef))
    (:show-stack ($ :show-stack (@undef)))
    (otherwise (if *quote* 
                   (@intern exp) 
                 (@symvalue exp)))))

(defun @eval (exps)
  (destructuring-bind (exp) exps
    ($ (compile-no-tail exp) :eval)))

(defun @compile-list (exp)
  (if *quote*
      (@list exp)
    (destructuring-bind (car . cdr) exp
      (case car
        (:quote (@quote cdr))
        (:if    (@if cdr))
        (:begin (@begin cdr))
        (:lambda (@lambda cdr))
        (:toplevel-lambda (@lambda cdr t)) ;; TODO: *toplevel*は不要かも
        (:define (@toplevel-define cdr))
        (:set! (@set! cdr))
        (:let (@let cdr))
        (:let* (compile-impl (@let*-expand cdr)))
        (:case (@case cdr))
        (:or (@or cdr))
        (:and (compile-impl (@and-expand cdr)))
        (:__apply (@list-apply cdr))
        (:__eval (@eval cdr))
        (otherwise 
         (@apply car cdr))))))

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

(defparameter *free-vars* '())
(defparameter *mutable-free-vars* '())
(defparameter *binded-vars* '())
(defun @inspect (exp)
  (let ((*free-vars* '())
        (*mutable-free-vars* '())
        (*binded-vars* '()))
    (@inspect-impl exp)
    (values *free-vars* *mutable-free-vars*)))

(defun @inspect-impl (exp)
  (etypecase exp
    ((or null integer string character))
    (symbol (@inspect-symbol exp))
    (list (@inspect-list exp))))

(defun @inspect-symbol (exp)
  (case exp
    ((:|true| :|false| :|undef| :show-stack))
    (otherwise 
     (unless (find exp *binded-vars*)
       (pushnew exp *free-vars*)))))

(defun @inspect-list (exp)
  (destructuring-bind (car . cdr) exp
    (case car
      (:quote)
      ((:if :begin) (mapcar #'@inspect-impl cdr))
      (:lambda (destructuring-bind (args . body) cdr
                 (let ((*binded-vars* (append (normalize-args args) *binded-vars*)))
                   (@inspect-impl `(:begin ,@body)))))
      (:define (destructuring-bind (var val) cdr
                 (@inspect-impl val)
                 (push var *binded-vars*)))  ; TODO: toplevel云々
      (:set! (destructuring-bind (var val) cdr
               (@inspect-impl val)
               (unless (find var *binded-vars*)
                 (pushnew var *free-vars*)
                 (pushnew var *mutable-free-vars*))))
      (:let (destructuring-bind (bindings . body) cdr
              (loop FOR (var val) IN bindings
                    DO (@inspect-impl val))
              (let ((*binded-vars* (append (mapcar #'car bindings) *binded-vars*)))
                (@inspect-impl `(:begin ,@body)))))
      (:let* (@inspect-impl (@let*-expand cdr)))
      (:case (@inspect-impl (@case-expand cdr)))
      (:or   (@inspect-impl (@or-expand cdr)))
      (:and  (@inspect-impl (@and-expand cdr)))
      (:__apply (mapcar #'@inspect-impl cdr))
      (otherwise
       (mapcar #'@inspect-impl exp)))))
