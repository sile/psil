(defpackage pvmi
  (:use :common-lisp)
  (:export octet octets))
(in-package :pvmi)

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(array octet))
