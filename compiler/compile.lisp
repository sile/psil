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

(defun @compile-symbol (sym)
  (case sym
    (:true @true)
    (:false @false)
    (otherwise
     (if *quote?*
         (@symbol sym)
       (if (assoc sym *bindings*)
           ($ :localref (cdr (assoc sym *bindings*)))
         ($ (@symbol sym) :symref))))))

(defun @compile-setval (var val)
  (if (assoc var *bindings*)
      ($ (compile-impl val) :localset (cdr (assoc var *bindings*)))
    ($ (compile-impl val) (@symbol var) :symset)))

(defun @if (cnd then else)
  (let* ((then^ (flatten (compile-impl then)))
         (else^ (flatten ($ (compile-impl else) (@int (length then^)) :jump))))
    ($ (compile-impl cnd) (@int (length else^)) :jump-if else^ then^)))

(defun @compile-let (bindings body)
  (let ((*bindings* (append (loop FOR (var) IN bindings
                                  COLLECT (cons var (1- (incf *local-var-index*))))
                            *bindings*)))
    ($ (loop FOR (var val) IN bindings
             COLLECT ($ (compile-impl val) :localset (cdr (assoc var *bindings*)) :drop))
       (compile-impl `(progn ,@body)))))

(defun @compile-list (exp)
  (if *quote?*
      (@list (mapcar #'compile-impl exp))
    (destructuring-bind (car . cdr) exp
      (case (intern (symbol-name car) :keyword)
        (:quote (let ((*quote?* t)) (compile-impl (car cdr))))
        (:if (destructuring-bind (cnd then &optional else) cdr
               (@if cnd then else)))
        (:let (destructuring-bind ((&rest bindings) &body body) cdr
                (@compile-let bindings body)))
        (:progn (let ((last (car (last cdr)))
                     (butlast (butlast cdr)))
                 ($ (mapcar #'compile-impl butlast) :dropn (length butlast) (compile-impl last))))
        (:lambda )
        (:macro-lambda )
        (:setval (destructuring-bind (var val) cdr
                   (@compile-setval var val)))
        (otherwise )))))

(defun compile-impl (exp)
  (etypecase exp
    (null    @nil)
    (fixnum (@int exp))
    (string (@str exp))
    (character (@char exp))
    (symbol (@compile-symbol exp))
    (list (@compile-list exp))))
