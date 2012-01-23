(defpackage psil.compile
  (:use :common-lisp :psil.bytecode)
  (:export compile-ops))
(in-package :psil.compile)

(defun flatten (list &aux acc)
  (labels ((recur (x)
             (when x
               (if (atom x)
                   (push x acc)
                 (destructuring-bind (car . cdr) x
                   (progn (recur car)
                          (recur cdr)))))))
    (recur list)
    (nreverse acc)))

(defun int-to-bytes (n)
  (declare ((signed-byte 32) n))
  (nreverse
   (loop FOR i FROM 3 DOWNTO 0
         COLLECT (ldb (byte 8 (* 8 i)) n))))

(defun resolve-label-refs (octets label-poses ref-poses)
  (loop FOR (pos . label) IN ref-poses
        FOR ref-pos = (cdr (assoc label label-poses))
    DO
    (setf (subseq octets pos (+ pos 4)) (nreverse (int-to-bytes (- ref-pos (+ 5 pos))))))
  octets)
        
(defun compile-ops-impl (ops octets label-poses ref-poses)
  (if (null ops)
      (let ((octets (coerce (nreverse octets) '(vector (unsigned-byte 8)))))
        (resolve-label-refs octets label-poses ref-poses))
    (destructuring-bind (op . rest) ops
      (etypecase op
        (integer (compile-ops-impl rest `(,@(int-to-bytes op) ,(op.sym->code :int) ,@octets) label-poses ref-poses))
        (symbol (compile-ops-impl rest (cons (op.sym->code op) octets) label-poses ref-poses))
        (cons
         (destructuring-bind (tag arg) op
           (ecase tag
             (:label (compile-ops-impl rest octets (acons arg (length octets) label-poses) ref-poses))
             ((:jump :when.jump :lambda)
              (compile-ops-impl rest 
                                `(,(op.sym->code tag) ,@(int-to-bytes 0) ,(op.sym->code :int) ,@octets) 
                                label-poses 
                                (acons (1+ (length octets)) arg ref-poses))))))))))

(defun compile-ops (ops)
  (compile-ops-impl ops '() '() '()))
