(defstruct (instruction (:conc-name "INS-"))
  (name t :type symbol)
  (code 0 :type fixnum))

(defun ins (name code)
  (make-instruction :name name :code code))

#|
[フォーマット]
magic: "psil"
header:
 version:
 lable-count:
 symbol-count:
 constant-count:
 code-size:
label:
 [name][id][position]
symbol-table:
 [name][value]
constant:
 [type][id][value]
code:

|#

(defparameter *instructions*
  (list
   ;; 00x
   (ins :int 001) ; int i4
   (ins :symval 002) ; symbol sym

   ;; 10x
   (ins :apply 101) ; arg... fn apply
   (ins :tail-apply 102) ; arg... fn apply
   (ins :return 103) ; return u1

   ;; 20x
   (ins :reserve 201) ; reserve u1
   (ins :drop 202) ; drop u1
   (ins :ref 203) ; ref i1

   ;; 30x
   (ins :halt 301) ; halt

   ;; 40x
   (ins :lambda 401) ; lambda u1 u1 i4 ...  # [closed var count] [arity] [local var count] [body length]

   ))

(defun name=>ins (name)
  (let ((ins (find name *instructions* :key #'ins-name)))
    (assert ins () "undefined instruction: ~a" name)
    ins))

(defun ins=>fun (ins)
  (let ((funname (format nil "_~a" (symbol-name (ins-name ins)))))
    (symbol-function (intern funname))))

(defstruct stack
  (buf #() :type vector)
  (pos 0 :type fixnum)
  (base 0 :type fixnum))

(defmethod print-object ((o stack) stream)
  (with-slots (buf pos base) o
    (format stream "~a" (subseq buf base pos))))

(defun get-stack (x)
  (etypecase x
    (stack x)
    (env (env-stack x))))

(defun spush (x stack &aux (stack (get-stack stack)))
  (with-slots (buf pos) stack
    (setf (aref buf pos) x)
    (incf pos)
    stack))

(defun spop (stack &aux (stack (get-stack stack)))
  (with-slots (buf pos) stack
    (assert (plusp pos))
    (decf pos)
    (aref buf pos)))

(defun sreserve (n stack &aux (stack (get-stack stack)))
  (incf (stack-pos stack) n))

(defun sdrop (n stack &aux (stack (get-stack stack)))
  (decf (stack-pos stack) n))

(defun sbase-drop (n stack &aux (stack (get-stack stack)))
  (decf (stack-base stack) n))

(defun sref (stack i &aux (stack (get-stack stack)))
  (with-slots (buf base pos) stack
    (aref buf (+ base i))))

(defun sset (stack i x &aux (stack (get-stack stack)))
  (with-slots (buf base pos) stack
    (setf (aref buf (+ base i)) x)))

(defstruct env 
  (pc 0)
  codes
  stack
  symbols)

(defun empty-env (codes)
  (make-env :codes codes
            :stack (make-stack :buf (make-array 1000) :pos 3)))

(defun read-ins (env)
  (prog1 (aref (env-codes env) (env-pc env))
    (incf (env-pc env))))

(defun read-int (env)
  (read-ins env))

(defun read-int8 (env)
  (read-int env))

(defun read-operand (env)
  (read-int env))

#|
[stack]
-- [pos]
一つ前のposへのリンク
一つ前のbaseへのリンク
リターンアドレス
-- [base]
x-y ローカル変数
n-x closes
0-n 引数

[conti]
stackの全コピー
|#

(defun exec (&rest codes)
  (catch :halt (exec-impl (empty-env (coerce (flatten codes) 'vector)))))

(defun exec-impl (env)
  (funcall (ins=>fun (name=>ins (read-ins env))) env)
  (exec-impl env))

(defun _halt (env)
  (throw :halt (env-stack env)))

(defun _int (env)
  (spush (read-int env) env))

(defstruct fun
  closes
  (arity 0)
  (local-num 0)
  body)

(defun _lambda (env)
  (spush (make-fun 
          :closes (loop REPEAT (read-int8 env) COLLECT (spop env))
          :arity (read-int8 env)
          :local-num (read-int8 env)
          :body (1+ (env-pc env)))
         env)
  (incf (env-pc env) (read-int env)))

(defun ready-frame (env prev-pos &aux (stack (env-stack env)))
  (let ((prev-base (stack-base stack)))
    (setf (stack-base stack) (stack-pos stack))
    
    (spush (env-pc env) env)
    (spush prev-base env)
    (spush prev-pos env)))

(defparameter *env* nil)

(defun _apply (env &aux (prev-pos (stack-pos (env-stack env))))
  (with-slots (closes arity local-num body) (spop env)
    (loop FOR v IN closes DO (spush v env))
    (loop REPEAT local-num DO (spush 0 env))
    (ready-frame env (- prev-pos arity 1))
    (etypecase body
      (integer (setf (env-pc env) body))
      (function (let ((*env* env)) ; native
                  (spush (funcall body) env))))))
       

(defun _return (env &aux (stack (env-stack env)))
  (let ((pc (sref env 0))
        (prev-base (sref env 1))
        (prev-pos (sref env 2))
        (return-value (sref env 3)))
    (setf (stack-base stack) prev-base)
    (setf (stack-pos stack) prev-pos)

    (spush return-value env)
    (setf (env-pc env) pc)))

(defun _symval (env) 
  (spush (symbol-value (read-operand env)) env))

(defun _ref (env)
  (spush (sref env (- (read-int8 env))) env))

(defun @var (i)
  (sref *env* (- i)))

(defun @set-var (i x)
  (sset *env* (- i) x))

(defun @ret (x)
  (spush x *env*))

(defmacro defnative (name args &body body)
  `(defparameter ,name
     (make-fun :arity ,(length args)
               :body (lambda ()
                       (let ,(loop FOR a IN args 
                                   FOR i FROM (length args) DOWNTO 0
                                   COLLECT `(,a (@var ,i)))
                         ,@body)))))

(defnative $add (x y)
  (+ x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
