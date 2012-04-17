(defpackage pvme
  (:use :common-lisp :pvmi)
  (:export init
           load-bc
           
           execute
           execute-from-file
           execute-from-stream
           ))
(in-package :pvme)
