(require :asdf)
(asdf:load-system :pvm)

(defun basename (pathstring)
  (let ((path (parse-namestring pathstring)))
    (format nil "~A~@[.~A~]" (pathname-name path) (pathname-type path))))

;; '(a b c &optional c &key (d e)) -> '(a b c d)
(defun collect-varsym (args)
  (mapcar (lambda (a)
            (if (consp a) (car a) a))
          (remove-if (lambda (a)
                       (and (symbolp a) (string= "&" a :end2 1)))
                     args)))

(defmacro main-lambda (args &body body)
  (let ((usage nil))
    (when (stringp (car body))
      (setf usage (car body)
	    body  (cdr body)))
    
    `(lambda ()
       ;;(sb-ext:disable-debugger)

       ;; When failed arguments destructuring, show documentation and exit
       ,(when usage
          `(handler-case 
            (destructuring-bind ,args (cdr sb-ext:*posix-argv*) 
              (declare (ignore ,@(collect-varsym args))))
            (error ()
              (format *error-output* "~&~?~%~%" 
                      ,usage
                      (list (basename (car sb-ext:*posix-argv*))))
              (sb-ext:quit :unix-status 1))))
       
       (destructuring-bind ,args (cdr sb-ext:*posix-argv*)
         (handler-case
          (locally ,@body)
          (sb-int:simple-stream-error ()
             (sb-ext:quit :unix-status 0 :recklessly-p t))
          (condition (c)
             (format *error-output* "~&ERROR: ~A~%" c)
             (sb-ext:quit :unix-status 1)))
         (sb-ext:quit :unix-status 0)))))

(sb-ext:save-lisp-and-die
 "pvm"
 :executable t
 :toplevel (main-lambda (&optional bytecode-file)
             "~A: [bytecode-file]"
             (format t "~&=> ~s~2%"
                     (pvm:execute-from-file (or bytecode-file "/dev/stdin")))))
