(defpackage pvm.stream
  (:use :common-lisp :psil-vm)
  (:export read-octet
           read-int32
           read-uint32
           jump
           pos
           last-offset
           eos?))
(in-package :pvm.stream)

(defun last-offset (env)
  (1- (length (pvm.env:bytecodes env))))

(defun eos? (env)
  (> (pvm.env:pc env) (last-offset env)))

(defun read-octet (env)
  (assert (not (eos? env)) () "End of stream reached")
  (let ((octet (aref (pvm.env:bytecodes env) (pvm.env:pc env))))
    (incf (pvm.env:pc env))
    octet))

(defun read-uint32 (env)
  (assert (<= (pos env) (- (last-offset env) 4)) () "End of stream reached")
  (let ((bytecodes (pvm.env:bytecodes env))
        (i (pos env)))
    (incf (pvm.env:pc env) 4)
    (+ (ash (aref bytecodes (+ i 3)) 24)
       (ash (aref bytecodes (+ i 2)) 16)
       (ash (aref bytecodes (+ i 1)) 08)
       (ash (aref bytecodes (+ i 0)) 00))))

(defun jump (env offset &key (base 0))
  (let ((new-pos (+ base offset)))
    (assert (<= 0 new-pos (last-offset env)) () "Out of range")
    (setf (pvm.env:pc env) new-pos)
    env))

(defun pos (env) 
  (pvm.env:pc env))
