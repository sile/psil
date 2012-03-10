(defpackage pvm.stream
  (:use :common-lisp :psil-vm)
  (:export read-octet
           read-int32 read-uint32
           read-int16 read-uint16
           read-int8  read-uint8
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

;; TODO: macro
(defun read-uint32 (env)
  (assert (<= (pos env) (- (last-offset env) 3)) () "End of stream reached")
  (let ((bytecodes (pvm.env:bytecodes env))
        (i (pos env)))
    (incf (pvm.env:pc env) 4)
    (+ (ash (aref bytecodes (+ i 0)) 24)
       (ash (aref bytecodes (+ i 1)) 16)
       (ash (aref bytecodes (+ i 2)) 08)
       (ash (aref bytecodes (+ i 3)) 00))))

(defun read-int32 (env)
  (let ((n (read-uint32 env)))
    (if (< n #x80000000)
        n
      (- n #x100000000))))

(defun read-uint16 (env)
  (assert (<= (pos env) (- (last-offset env) 1)) () "End of stream reached")
  (let ((bytecodes (pvm.env:bytecodes env))
        (i (pos env)))
    (incf (pvm.env:pc env) 2)
    (+ (ash (aref bytecodes (+ i 0)) 08)
       (ash (aref bytecodes (+ i 1)) 00))))

(defun read-int16 (env)
  (let ((n (read-uint16 env)))
    (if (< n #x8000)
        n
      (- n #x10000))))

(defun read-uint8 (env)
  (read-octet env))

(defun read-int8 (env)
  (let ((n (read-uint8 env)))
    (if (< n #x80)
        n
      (- n #x100))))

(defun jump (env offset &key (base 0))
  (let ((new-pos (+ base offset)))
    (assert (<= 0 new-pos (last-offset env)) () "Out of range")
    (setf (pvm.env:pc env) new-pos)
    env))

(defun pos (env) 
  (pvm.env:pc env))
