(defpackage plc
  (:use :common-lisp)
  (:shadow :common-lisp compile compile-file)
  (:export parse
           parse-file
           parse-string
           compile
           compile-file
           compile-string))
(in-package :plc)
