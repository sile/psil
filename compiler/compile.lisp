(in-package :plc)

(defun @int (n)
  (list (i :int) (int-to-bytes n)))

(defun @str (s)
  (let ((o (sb-ext:string-to-octets s)))
    (list (i :string) (int-to-bytes (length o)) (coerce o 'list))))

(defun @char (ch)
  (list (i :char) (int-to-bytes (char-code ch))))

(defun @symbol (symbol)
  (let* ((name (symbol-name symbol))
         (o (sb-ext:string-to-octets name)))
    (list (i :symbol) (length o) (coerce o 'list))))

(defparameter @nil (i :nil))
(defparameter @true (i :true))
(defparameter @false (i :false))

(defvar *quote?*)
(defvar *bindings*)

(defun compile-impl (exp)
  (etypecase exp
    (null    @nil)
    (fixnum (@int exp))
    (string (@str exp))
    (character (@char exp))
    (symbol (case exp
              (:true @true)
              (:false @false)
              (otherwise
               (if *quote?*
                   (@symbol exp)
                 (if (assoc exp *bindings*)
                     (list (i :localref) (cdr (assoc exp *bindings*)))
                   (list (@symbol exp) (i :symref)))))))
    ))
