(defpackage pvm
  (:use :common-lisp)
  (:export execute
           execute-from-file
           execute-from-list))
(in-package :pvm)

(deftype int4 () '(signed-byte 32))

