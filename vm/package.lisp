(defpackage pvm
  (:use :common-lisp)
  (:export octet octets

           execute execute-from-file
           write-bytecodes-to-file
           ))
(in-package :pvm)

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(array octet))
