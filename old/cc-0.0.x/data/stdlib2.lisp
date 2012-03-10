(defmacro push (val list)
  `(progn (setq ,list (cons ,val ,list))
          ,list))

(setq *interfaces* '())
(setq *interface-fns* '())
(defmacro def-interface (name &rest defs)
  `(progn
     (push '(,name ,defs) *interfaces*)
     (push `(,name) *interface-fns*)
     'done))

(defun find-if (fn list)
  (if (null list)
      nil
    (if (fn (car list))
        (car list)
      (find-if fn (cdr list)))))

(defun find-interface (name)
  (find-if (lambda (x) (eq name (car x))) *interfaces*)) 

(defmacro implement (interface-name name &rest defs)
  `(if (null (find-interface ',name))
    nil
    t))   
