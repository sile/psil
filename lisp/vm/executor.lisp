(defpackage pvm.executor
  (:use :common-lisp :psil-vm)
  (:export execute-from-file
           execute-from-stream
           execute-from-octets))
(in-package :pvm.executor)

(defun execute (env)
  (if (pvm.stream:eos? env)
      env
    (let ((op (pvm.op:read-op env)))
      (execute (funcall op env)))))

(defun execute-from-octets (octets &optional env)
  (execute (pvm.env:init octets env)))

(defun execute-from-stream (stream &optional env)
  (let ((octets (loop FOR o = (read-byte stream nil nil)
                      WHILE o 
                      COLLECT o INTO list
                      FINALLY
                      (return (coerce o 'octets)))))
    (execute-from-octets octets env)))

(defun execute-from-file (path &optional env)
  (with-open-file (in path :element-type 'octet)
    (execute-from-stream in env)))
