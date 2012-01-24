(defpackage psil-vm.type
  (:use :common-lisp :psil-vm)
  (:nicknames :pvm.type)
  (:export %int %int-value))
(in-package :psil-vm.type)

(defstruct %root)

(defstruct (%int (:include %root)
                 (:constructor %int (value)))
  (value 0 :type int32))
(defmethod print-object ((o %int) stream)
  (format stream "~a:%INT" (%int-value o)))

