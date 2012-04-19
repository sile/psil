(in-package :plc)

(defun blf-call (sym)
  ($ (@symbol sym) 50 101))

(defmacro $ (&rest exps)
  `(list ,@(loop FOR e IN exps
                 COLLECT (typecase e
                           (keyword `(i ,e))
                           (symbol `(blf-call ,e))
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
    ($ :symbol (length o) (coerce o 'list))))

(defun @list (elems)
  ($ elems :list (length elems))) 

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

(defun @compile-list (exp)
  (if *quote?*
      (@list (mapcar #'compile-impl exp))
    (destructuring-bind (car . cdr) exp
      (case car
        (quote (let ((*quote?* t)) (compile-impl cdr)))
        (if )
        (let )
        (progn )
        (lambda )
        (macro-lambda )
        (setq )
        (otherwise )))))

(defun compile-impl (exp)
  (etypecase exp
    (null    @nil)
    (fixnum (@int exp))
    (string (@str exp))
    (character (@char exp))
    (symbol (@compile-symbol exp))
    (list (@compile-list exp))))
