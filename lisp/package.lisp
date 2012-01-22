(defpackage psil
  (:use :common-lisp)
  (:export exec
           execute-bytecode
           execute-bytecode-from-file
           execute-bytecode-from-octets))
(in-package :psil)

(deftype octet () '(unsigned-byte 8))
