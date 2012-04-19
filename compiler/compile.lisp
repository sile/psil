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
  (let* ((name (symbol-name symbol))
         (o (sb-ext:string-to-octets name)))
    ($ :symbol (short-to-bytes (length o)) (coerce o 'list))))

(defun @list (elems)
  ($ elems :list (int-to-bytes (length elems))))

(defparameter @nil ($ :nil))
(defparameter @true ($ :true))
(defparameter @false ($ :false))

(defvar *quote?*)
(defvar *bindings*)
(defvar *local-var-index*)
(defvar *scope*)

(defstruct local-bind
  scope
  name
  index)

(defun local-bind (name index)
  (make-local-bind :scope *scope*
                   :name name
                   :index index))

(defun find-local-bind (var)
  (find var *bindings* :key #'local-bind-name))

(defun local-index (var)
  (local-bind-index (find-local-bind var)))

(defmacro with-env ((&key (local-var-offset 0)) &body body)
  `(let ((*scope* (gensym))
         (*quote?* nil)
         (*bindings* '())
         (*local-var-index* ,local-var-offset))
     ,@body))

(defun @compile-symbol (sym)
  (case sym
    (:true @true)
    (:false @false)
    (otherwise
     (if *quote?*
         (@symbol sym)
       (if (find-local-bind sym)
           ($ :localref (local-index sym))
         ($ (@symbol sym) :symref))))))

(defun @compile-setval (var val)
  (if (find-local-bind var)
      ($ (compile-impl val) :localset (local-index var))
    ($ (compile-impl val) (@symbol var) :symset)))

(defun @if (cnd then else)
  (let* ((then^ (flatten (compile-impl then)))
         (else^ (flatten ($ (compile-impl else) (@int (length then^)) :jump))))
    ($ (compile-impl cnd) (@int (length else^)) :jump-if else^ then^)))

(defun @compile-let (bindings body)
  (let ((*bindings* (append (loop FOR (var) IN bindings
                                  COLLECT (local-bind var (1- (incf *local-var-index*))))
                            *bindings*)))
    ($ (loop FOR (var val) IN bindings
             COLLECT ($ (compile-impl val) :localset (local-index var) :drop))
       (compile-impl `(progn ,@body)))))

(defun @compile-funcall (fun args)
  ($ (mapcar #'compile-impl args) (compile-impl fun) :apply))

#|
- 外側のスコープの引数/ローカル変数一覧
- 内側のスコープの引数/ローカル変数一覧  
- 内側で参照している変数一覧
|#
(defun inspect-var-info (bindings exp) ;; bindings = ((var rd-count wr-count))
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
                         (mapcar #'recur (mapcar #'cdr binds))
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

(defun @compile-lambda (args body)
  (multiple-value-bind (closed-vars local-var-count)
                       (inspect-var-info (loop FOR a IN args COLLECT (list a 0 0))
                                         body)
    (declare (ignore closed-vars local-var-count))
    
    ))
#|
(defun make-fun-body (args body)
  (let* ((local-var-count (count-local-var (cons 'progn body)))
         (*bindings* (loop FOR a IN (reverse args)
                           FOR i FROM local-var-count
                           COLLECT (cons a i)))
         (*local-var-start* 0))

    ;; returnがimplicit-prognを兼ねているかも
    (list local-var-count ($ (compile-bc (cons 'progn body)) 103))))

(defun @defun (name args body)
  (destructuring-bind (local-var-count body-bc) (make-fun-body args body)
    (let ((fn ($ 201 0 (length args) local-var-count (int-to-bytes (length body-bc)) body-bc)))
      ($ fn (@symbol name) 51))))
|#

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
                   (@compile-lambda args body)))
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
