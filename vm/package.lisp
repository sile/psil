(defpackage pvm
  (:use :common-lisp)
  (:export octet octets
           ))
(in-package :pvm)

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(array octet))
