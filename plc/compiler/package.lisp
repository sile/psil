(defpackage plcc
  (:use :common-lisp)
  (:shadow :common-lisp compile compile-file)
  (:export compile
           compile-file))
(in-package :plcc)
