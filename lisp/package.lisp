(defpackage psil
  (:use :common-lisp)
  (:shadow :common-lisp read read-from-string eval)
  (:export read 
           read-from-string
           eval))
(in-package :psil)
