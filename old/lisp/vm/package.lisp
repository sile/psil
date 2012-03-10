(defpackage psil-vm
  (:use :common-lisp)
  (:nicknames :pvm)
  (:export exec
            
           *fastest*
           octet
           octets
           int32 uint32))
(in-package :psil-vm)

(defparameter *fastest* '(optimize (speed 3) (safety 0) (debug 0)))

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(simple-array octet))
(deftype int32 () '(signed-byte 32))
(deftype uint32 () '(unsigned-byte 32))
