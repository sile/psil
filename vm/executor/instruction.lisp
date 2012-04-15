(in-package :pvme)

(defstruct ins
  (code 0 :type fixnum)
  (fun  t :type symbol))

(defun ins (code fun)
  (make-ins :code code :fun fun))

(defparameter *ins-list*
  (list 
   ;; 00x 
   (ins 001 '_int)
   (ins 002 '_string)
   (ins 003 '_array)
   (ins 004 '_char)
   (ins 005 '_symbol)

   ;; 05x
   (ins 050 '_symref)
   (ins 051 '_symset)

   ;; 10x
   (ins 101 '_apply)
   (ins 102 '_tail-apply) ; TODO
   (ins 103 '_return)

   ;; 20x
   (ins 201 '_lambda)
   (ins 202 '_localref)
   (ins 203 '_localset)
   ))

(defun find-ins (code)
  (let ((ins (find code *ins-list* :key #'ins-code)))
    (assert ins () "No instruction found: code=~a" code)
    ins))

(defun execute-op (opcode)
  (funcall (ins-fun (find-ins opcode))))

(defun execute-one (in)
  (execute-op (read-op in)))

(defstruct fun
  (closed-vals nil :type list)
  (arity 0 :type fixnum)
  (local-var-count 0 :type fixnum)
  (body 0 :type (or fixnum function)))

(define-symbol-macro +in+ (env-code-stream *env*))
(define-symbol-macro +stack+ (env-stack *env*))
(define-symbol-macro +symbols+ (env-symbols *env*))

;; 00x
(defun _int ()
  (spush +stack+ (read-int +in+)))

(defun _string ()
  (let ((len (read-int +in+)))
    (spush +stack+ (read-string +in+ len))))

(defun _array ()
  (let ((len (read-int +in+)))
    (spush +stack+
           (coerce (loop REPEAT len COLLECT (progn (execute-one +in+) (spop +stack+))) 
                   'vector))))

(defun _char ()
  (spush +stack+ (code-char (read-uint +in+))))

(defun _symbol ()
  (let* ((name (progn (execute-one +in+) (spop +stack+)))
         (sym (intern  name :keyword)))
    (set-symbol-value +symbols+ sym nil)
    (spush +stack+ sym)))

;; 05x
(defun _symref ()
  (spush +stack+ (get-symbol-value +symbols+ (spop +stack+))))

;; value symbol SYMSET
(defun _symset ()
  (let ((sym (spop +stack+))
        (val (spop +stack+)))
    (set-symbol-value +symbols+ sym val)
    (spush +stack+ val)))

;; 10x
(defun _apply ()
  (with-slots (closed-vals arity local-var-count body)
              (the fun (spop +stack+))
    (create-frame +stack+ closed-vals local-var-count (get-pc +in+))
    (etypecase body
      (fixnum (set-pc +in+ body))
      (function (spush +stack+ (funcall body *env*))))))

(defun _return ()
  (multiple-value-bind (address value) (destroy-frame +stack+)
    (set-pc +in+ address)
    (spush +stack+ value)))

;; 20x
;; CLOSE-VALUE* lambda CLOSE-VAL-COUNT:byte ARITY:byte LOCAL-VAR-COUNT:byte BODY-LENGTH BODY-BEGIN
(defun _lambda ()
  (let* ((closed-count (read-ubyte +in+))
         (arity (read-ubyte +in+))
         (local-var-count (read-ubyte +in+))
         (body-size (read-int +in+))
         (fun (make-fun :closed-vals (loop REPEAT closed-count COLLECT (spop +stack+))
                        :arity arity
                        :local-var-count local-var-count
                        :body (get-pc +in+))))
    (set-pc +in+ (+ (get-pc +in+) body-size))
    (spush +stack+ fun)))

(defun _localref ()
  (let ((i (read-ubyte +in+)))
    (spush +stack+ (local-ref +stack+ i))))

(defun _localset ()
  (let ((i (read-ubyte +in+)))
    (spush +stack+ (local-set +stack+ i (spop +stack+)))))
