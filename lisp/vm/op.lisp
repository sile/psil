(defpackage psil-vm.op
  (:use :common-lisp :psil-vm)
  (:nicknames :pvm.op)
  (:export read-op))
(in-package :psil-vm.op)

(defun read-op (env)
  ;; TODO:
  (pvm.stream:read-octet env))
