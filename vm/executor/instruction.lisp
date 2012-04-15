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
   
   ))

(defun find-ins (code)
  (let ((ins (find code *ins-list* :key #'ins-code)))
    (assert ins () "No instruction found: code=~a" code)
    ins))

(defun execute-op (opcode)
  (funcall (ins-fun (find-ins opcode))))

(defun execute-one (in)
  (execute-op (read-op in)))

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
