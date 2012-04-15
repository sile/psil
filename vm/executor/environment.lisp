(in-package :pvme)

(defstruct env
  (code-stream t :type octets-stream)
  (stack       t :type stack)
  (symbols     t :type symbol-table))

(defparameter *env* 
  (make-env :stack (create-stack)
            :symbols (create-symbol-table)
            :code-stream (make-code-stream (make-array 0 :element-type 'octet))))

(defmacro with-new-env ((code-stream) &body body)
  `(let ((*env* (make-env :stack (create-stack)
                          :symbols (env-symbols *env*)
                          :code-stream ,code-stream)))
     ,@body))

