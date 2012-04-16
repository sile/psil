(in-package :pvme)

(defmacro defnative (name args &body body)
  `(defparameter ,name
     (make-fun :arity ,(length args)
               :body (lambda ()
                       (let ,(loop FOR a IN args 
                                   FOR i FROM (1- (length args)) DOWNTO 0
                                   COLLECT `(,a (local-ref +stack+ ,i)))
                         (spush +stack+ (locally ,@body)))))))

(defnative $add (x y)
  (+ x y))

(defparameter *natives*
  `(
    (:+ ,$add)
    ))
