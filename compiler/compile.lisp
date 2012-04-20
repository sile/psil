(in-package :plc)

;;(defun blf-call (sym)
;;  ($ (@symbol sym) 50 101))

(defmacro $ (&rest exps)
  `(list ,@(loop FOR e IN exps
                 COLLECT (typecase e
                           (keyword `(i ,e))
;;                           (symbol `(blf-call ,e))
                           (t e)))))
(defun @int (n)
  ($ :int (int-to-bytes n)))

(defun @str (s)
  (let ((o (sb-ext:string-to-octets s)))
    ($ :string (int-to-bytes (length o)) (coerce o 'list))))

(defun @char (ch)
  ($ :char (int-to-bytes (char-code ch))))

(defun @symbol (symbol)
  ($ :constref (short-to-bytes (constant-index symbol))))

(defun @list (elems)
  ($ elems :list (int-to-bytes (length elems))))

(defparameter @nil ($ :nil))
(defparameter @true ($ :true))
(defparameter @false ($ :false))

(defvar *quote?*)
(defvar *bindings*)
(defvar *local-var-index*)
(defvar *scope*)
(defvar *in-root-scope*)

(defvar *constant-table* nil)

(defun constant-index (value)
  (if #1=(gethash value *constant-table*)
      #1#
    (setf #1# (hash-table-count *constant-table*))))

(defstruct local-bind
  scope
  name
  index
  read-only)

(defun local-bind (name index read-only)
  (make-local-bind :scope *scope*
                   :name name
                   :index index
                   :read-only read-only))

(defun find-local-bind (var)
  (find var *bindings* :key #'local-bind-name))

(defun local-index (var)
  (local-bind-index (find-local-bind var)))

(defmacro with-env ((&key (local-var-offset 0) (bindings '())) &body body)
  `(let ((*scope* (gensym))
         (*quote?* nil)
         (*bindings* ,bindings)
         (*in-root-scope* t)
         (*constant-table* (or *constant-table* (make-hash-table)))
         (*local-var-index* ,local-var-offset))
     ,@body))

(defmacro no-root-scope (&body body)
  `(let ((*in-root-scope* nil)) ,@body))

(defun @compile-symbol (sym)
  (case sym
    (:true @true)
    (:false @false)
    (otherwise
     (if *quote?*
         (@symbol sym)
       (let ((v (find-local-bind sym)))
         (if v
             (if (local-bind-read-only v)
                 ($ :localref (local-index sym))
               ($ :refref (local-index sym)))
           ($ (@symbol sym) :symref)))))))

(defun @compile-setval (var val)
  (let ((v (find-local-bind var)))
    (if v
        (if (local-bind-read-only v) ; XXX: 名前が不適切
            ($ (compile-impl val) :localset (local-index var))
          ($ (compile-impl val) :refset (local-index var)))
      ($ (compile-impl val) (@symbol var) :symset))))

(defun @if (cnd then else)
  (let* ((then^ (flatten (compile-impl then)))
         (else^ (flatten ($ (compile-impl else) (@int (length then^)) :jump))))
    ($ (compile-impl cnd) (@int (length else^)) :jump-if else^ then^)))

(defun @compile-let (bindings body)
  (let* ((vars (mapcar #'car bindings))
         (w-closed-vars (inspect2 vars `(progn ,@body)))
         (*bindings* (append (loop FOR (var) IN bindings
                                   COLLECT (local-bind var (1- (incf *local-var-index*))
                                                       (not (find var w-closed-vars))))
                             *bindings*)))
    ($ (loop FOR (var val) IN bindings
             COLLECT ($ (adjust-args (list var)) ; XXX
                        (@compile-setval var val) :drop))
       (compile-impl `(progn ,@body)))))

(defun @compile-funcall (fun args)
  (if *in-root-scope*
      ($ (mapcar #'compile-impl args) (compile-impl fun) :tail-apply)
    ($ (mapcar #'compile-impl args) (compile-impl fun) :apply)))

#|
- 外側のスコープの引数/ローカル変数一覧
- 内側のスコープの引数/ローカル変数一覧  
- 内側で参照している変数一覧
|#
(defun inspect-var-info (bindings exp)
  (let ((local-vars '()))
    (let ((bindings bindings))
    (labels ((rd-refer (var)
               (let ((pair (assoc var bindings)))
                 (when pair (incf (second pair)))))
             (wr-refer (var)
               (let ((pair (assoc var bindings)))
                 (when pair 
                   (incf (second pair))
                   (incf (third pair)))))
             
             (recur (exp)
               (typecase exp
                 (null)
                 (symbol (rd-refer exp))
                 (list (recur-list (car exp) (cdr exp)))))
                 
             (recur-list (car cdr)
               (case (intern (symbol-name car) :keyword)
                 (:quote)
                 (:if (destructuring-bind (cnd then &optional else) cdr
                        (recur cnd) (recur then) (recur else)))
                 (:let (destructuring-bind ((&rest binds) &body body) cdr
                         (setf local-vars (append (mapcar #'car binds) local-vars))
                         (setf bindings (append (loop FOR (v) IN binds
                                                      COLLECT (list v 0 0))
                                                bindings))
                         (mapcar #'recur (mapcar #'second binds))
                         (mapcar #'recur body)))
                 (:progn (mapcar #'recur cdr))
                 (:lambda (destructuring-bind ((&rest args) &body body) cdr
                            (inspect-var-info (append (loop FOR a IN args COLLECT (list a 0 0))
                                                      bindings)
                                              `(progn ,@body))))
                 (:setval (destructuring-bind (var val) cdr
                            (wr-refer var);;(recur var) 
                            (recur val)))
                 (otherwise 
                  (mapcar #'recur cdr))))
             )
      (recur exp)))
    (values (loop FOR (var rd wr) IN bindings WHEN (plusp rd) 
                  COLLECT (list var (if (zerop wr) :read :write)))
            (length local-vars))))

(defun inspect2 (vars body)
  ;; TODO: 今は暫定的に一つでも書き込みがあるものは、参照にしてしまっている
  ;;  => closeされている & どこかで書き込みがある、もののみに限定する
  (let ((closed-vars (inspect-var-info (loop FOR v IN vars COLLECT (list v 0 0)) body)))
    (loop FOR (name type) IN closed-vars WHEN (eq type :write) COLLECT name)))

(defun adjust-args (args)
  (loop FOR a IN args
        FOR v = (find-local-bind a)
        WHEN (not (local-bind-read-only v))
        COLLECT ($ :localref (local-index a) :mkref (local-index a) :drop)))

(defun @compile-lambda (args body &aux (args (reverse args)) (w-closed-args (inspect2 args body)))
  (multiple-value-bind (closed-vars local-var-count)
                       (inspect-var-info (loop FOR b IN *bindings* 
                                               COLLECT (list (local-bind-name b) 0 0))
                                         body)
    (let ((close-indices (mapcar #'local-index (mapcar #'car closed-vars))))
     (with-env (:bindings (append (loop FOR a IN args
                                       FOR i FROM (+ (length closed-vars) local-var-count)
                                       COLLECT (local-bind a i (not (find a w-closed-args))))
                                  (loop FOR (name) IN closed-vars
                                        FOR v = (find-local-bind name)
                                        FOR i FROM local-var-count
                                        COLLECT (local-bind name i (local-bind-read-only v)))
                                 *bindings*)
               :local-var-offset (length closed-vars))
      (let* ((pre (adjust-args args))
             (body^ (flatten ($ pre (compile-impl body)))))
        ($ (mapcar (lambda (i) ($ :localref i)) close-indices)
           :lambda (length closed-vars) (length args)
           local-var-count (int-to-bytes (1+ (length body^))) body^ :return))))))

(defun @compile-list (exp)
  (if *quote?*
      (@list (mapcar #'compile-impl exp))
    (destructuring-bind (car . cdr) exp
      ;; XXX: schemeの場合は、car部がシンボルではない可能性がある => 保留
      (case (intern (symbol-name car) :keyword)
        (:quote (let ((*quote?* t)) (compile-impl (car cdr))))
        (:if (destructuring-bind (cnd then &optional else) cdr
               (@if cnd then else)))
        (:let (destructuring-bind ((&rest bindings) &body body) cdr
                (@compile-let bindings body)))
        (:progn (let ((last (car (last cdr)))
                      (butlast (butlast cdr)))
                 ($ (mapcar #'compile-impl butlast) :dropn (length butlast) (compile-impl last))))
        (:lambda (destructuring-bind ((&rest args) &body body) cdr
                   (@compile-lambda args `(progn ,@body))))
        ;;  (:macro-lambda ) => コンパイラとランタイムを分離している限りマクロは難しいので保留
        (:setval (destructuring-bind (var val) cdr
                   (@compile-setval var val)))
        (otherwise (@compile-funcall car cdr))))))

(defun compile-impl (exp)
  (etypecase exp
    (null    @nil)
    (fixnum (@int exp))
    (string (@str exp))
    (character (@char exp))
    (symbol (@compile-symbol exp))
    (list (@compile-list exp))))


;; TODO: 末尾になりえない箇所では、compile-implの代わりにこちらを使用する
(defun no-root-compile-impl (exp)
  (no-root-scope (compile-impl exp)))
