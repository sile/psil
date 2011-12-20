(defpackage psil.evaluator
  (:use :common-lisp)
  (:shadow :common-lisp eval)
  (:export eval))
(in-package :psil.evaluator)

(defun eval (exp)
  exp)

