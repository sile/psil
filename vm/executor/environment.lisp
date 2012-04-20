(in-package :pvme)

(defstruct env
  (code-stream t :type octets-stream)
  (stack       t :type stack)
  (consts      t :type simple-vector)
  (symbols     t :type symbol-table))

(defparameter *env* 
  (make-env :stack (create-stack)
            :symbols (create-symbol-table)
            :consts (make-array 0)
            :code-stream (make-code-stream (make-array 0 :element-type 'octet))))

(defmacro with-new-env ((code-stream constant-table) &body body)
  `(let ((*env* (make-env :stack (create-stack)
                          :symbols (env-symbols *env*)
                          :consts ,constant-table
                          :code-stream ,code-stream)))
     ,@body))

