(defpackage plcc
  (:use :common-lisp)
  (:shadow :common-lisp compile compile-file)
  (:export compile
           compile-file
           compile-string))
(in-package :plcc)
