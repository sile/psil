(defpackage pvm-bc
  (:use :common-lisp :pvmi)
  (:export header
           bc-codes
           read-from-file
           read-from-stream))
(in-package :pvm-bc)
