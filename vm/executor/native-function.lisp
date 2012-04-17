(in-package :pvme)

(defmacro defnative (name args &body body)
  `(defparameter ,name
     (make-fun :arity ,(length args)
               :body (lambda ()
                       (let ,(loop FOR a IN args 
                                   FOR i FROM (1- (length args)) DOWNTO 0
                                   COLLECT `(,a (local-ref +stack+ ,i)))
                         (spush +stack+ (locally ,@body)))))))

(defnative $add (x y) (+ x y))
(defnative $sub (x y) (- x y))
(defnative $mul (x y) (* x y))
(defnative $div (x y) (/ x y))
(defnative $mod (x y) (mod x y))

(defnative $make-array (size) (make-array size))
(defnative $ary-ref (ary i) (aref ary i))
(defnative $ary-set (ary i value) (setf (aref ary i) value))
(defnative $ary-len (ary) (length ary))

(defnative $cons (car cdr) (cons car cdr))
(defnative $car (cons) (car cons))
(defnative $cdr (cons) (cdr cons))

(defnative $load-bytecode-file (path) (load-bc path))

(defmacro unix-call (name &rest args)
  `(multiple-value-bind (fd err) (,(find-symbol (symbol-name name) :sb-unix) ,@args)
     (when err
       (format *error-output* "~&; ~a) err[~a]: ~a~%" ',name err (sb-int:strerror err)))
     fd))

(defnative $open (path flags mode) 
  (let ((fd (unix-call unix-open path flags mode)))
    (and fd (sb-sys:make-fd-stream fd :element-type 'octet :input t :output t))))

(defnative $close (stream) (close stream))
(defnative $read-byte (stream) (read-byte stream))
(defnative $write-byte (stream byte) (write-byte stream byte))
(defnative $read-chunk (stream buffer start length) 
  (read-sequence buffer stream :start start :end (+ start length)))
(defnative $write-chunk (stream buffer start length)
  (write-sequence buffer stream :start start :end (+ start length)))

(defparameter *natives*
  `(
    (:$+ ,$add)
    (:$- ,$sub)
    (:$* ,$mul)
    (:$/ ,$div)
    (:$mod ,$mod)

    (:$make-array ,$make-array)
    (:$ary-ref ,$ary-ref)
    (:$ary-set ,$ary-set)
    (:$ary-len ,$ary-len)

    (:$cons ,$cons)
    (:$car ,$car)
    (:$cdr ,$cdr)

    (:$load-bytecode-file ,$load-bytecode-file)

    (:$open ,$open)
    (:$close ,$close)
    (:$read-byte ,$read-byte)
    (:$write-byte ,$write-byte)
    (:$read-chunk ,$read-chunk)
    (:$write-chunk ,$write-chunk)
    (:$O_RDONLY ,sb-unix:o_rdonly)
    (:$O_WRONLY ,sb-unix:o_wronly)
    (:$O_RDWR ,sb-unix:O_RDWR)
    (:$O_CREAT ,sb-unix:O_CREAT)
    ))

;;;;;
(defun sym (sym)
  (let ((name (symbol-name sym)))
    `(0 4 0 ,(length name) ,@(map 'list #'char-code name))))

(defun call (sym)
  `(,@(sym sym) 0 50 0 101))
