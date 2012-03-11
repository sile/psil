(in-package :pvm)

(defmacro a.if (condition then else)
  `(let ((it ,condition))
     (if it
         ,then
       ,else)))

(defun symb (&rest args)
  (intern (format nil "~{~a~}" args)))
