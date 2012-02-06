(asdf:load-system :cl-asm)

(defpackage pvm-execute
  (:use :common-lisp :sb-alien)
  (:nicknames :pvme)
  (:export execute))
(in-package :pvm-execute)

(deftype octet () '(unsigned-byte 8))

(defun read-op (in)
  (read-byte in nil nil))

(defun read-uint (in)
  (+ (ash (read-byte in) 00)
     (ash (read-byte in) 08)
     (ash (read-byte in) 16)
     (ash (read-byte in) 24)))

(defun read-int (in)
  (let ((n (read-uint in)))
    (if (< n #x80000000)
        n
      (- n #x100000000))))

(defun read-bytecodes (in)
  (loop FOR pos = (file-position in)
        FOR op = (read-op in)
        WHILE op
    COLLECT
    (list 
     pos
     (ecase op
       (1 `(@int ,(read-int in)))
       (2 '(@add))
       (3 '(@sub))
       (4 :mul (error "unsupported"))
       (5 :div (error "unsupported"))
       (6 :mod (error "unsupported"))
       (7 '(@eql))
       (8 '(@less))
       (9 '(@dup))
       (10 '(@drop))
       (11 '(@swap))
       (12 '(@over))
       (13 '(@rot))
       (14 :rpush (error "unsupported"))
       (15 :rpop (error "unsupported"))
       (16 :rcopy (error "unsupported"))
       (17 '(unresolve @jump))    ;
       (18 '(unresolve @jump-if)) ; 
       (19 '(unresolve @call))    ;
       (20 '(@return))))))

(defun save-registers ()
  '(:progn
    (:push %rbp) (:mov %rbp %rsp) (:push %rdi) (:push %rsi) (:push %rbx)))

(defun restore-registers ()
  '(:progn
    (:pop %rbx) (:pop %rsi) (:pop %rdi) (:pop %rbp)))

(defun ready-data-stack ()
   '(:progn (:push %rax)
            (:push %rdi)
            (:mov %edi 102400)
            (:mov %rax (:extern "malloc"))
            (:call %rax)
            (:mov %rcx %rax)
            (:pop %rdi)
            (:pop %rax)))

(defun destroy-data-stack ()
  '(:progn (:push %rax)
           (:push %rdi)
           (:mov %rdi %rcx)
           (:mov %rax (:extern "free"))
           (:call %rax)
           (:pop %rdi)
           (:pop %rax)))

(defmacro body (&rest mnemonics)
  `'(,(save-registers)
     ,(ready-data-stack)
     ,@mnemonics
     ,(destroy-data-stack)
     ,(restore-registers)
     :ret))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defop (name args &body body)
    `(defmacro ,name ,args
       (list 'quote (locally ,@body)))))

 ;; ecx: data-stack
 ;; eax, ebx: temporary

(defop @ds-get (dst index) `(:mov ,dst (:refd %rcx ,(* index -4))))
(defop @ds-set (index src) `(:mov (:refd %rcx ,(* index -4)) ,src))
(defop @ds-inc (&optional (n 1)) `(:add %rcx ,(* 4 n)))
(defop @ds-dec (&optional (n 1)) `(:sub %rcx ,(* 4 n)))
  
(defop @push (src)
  `(:progn (@ds-inc)
           (@ds-set 0 ,src)))

(defop @pop (dst)
  `(:progn (@ds-get ,dst 0)
           (@ds-dec)))

(defop @pop2 (dst1 dst2)
  `(:progn (@ds-get ,dst1 0)
           (@ds-get ,dst2 1)
           (@ds-dec 2)))

(defop @swap-impl (index1 index2)
  `(:progn (@ds-get %eax ,index1)
           (@ds-get %ebx ,index2)
           (@ds-set ,index1 %ebx)
           (@ds-set ,index2 %eax)))

(defop @swap ()
  '(@swap-impl 0 1))

(defop @dup ()
  `(:progn (@ds-get %eax 0)
           (@push %eax)))

(defop @drop ()
  '(@ds-dec))

(defop @over ()
  `(:progn (@ds-get %eax 1)
           (@push %eax)))

(defop @rot ()
  `(:progn (@swap-impl 2 0)
           (@swap-impl 1 2)))

(defop @add ()
  `(:progn (@pop2 %ebx %eax)
           (:add %eax %ebx)
           (@push %eax)))

(defop @sub ()
  `(:progn (@pop2 %ebx %eax)
            (:sub %eax %ebx)
            (@push %eax)))

(defop @eql ()
  `(:progn (@pop2 %ebx %eax)
           (:sub %eax %ebx)
           (@push %eax)))
 
(defop @less ()
  `(:progn (@pop2 %ebx %eax)
           (:cmp %eax %ebx)
           (:mov %eax 0)
           (:setl %al)
           (@push %eax)))

(defop @jump-if (pos)
  `(:progn (@pop %eax)
           (:cmp %eax 0)
           (:jne ,pos)))

(defop @jump (pos)
  `(:jmp ,pos))

(defop @call (pos)
  `(:call ,pos))

(defop @int (n)
  `(@push ,n))

(defop @return ()
  :ret)

(defun symb (&rest args)
  (intern (format nil "~{~a~}" args)))

(defun resolve-addrs (bytecodes)
  (labels ((recur (list acc addrs)
             (if (null list)
                 (values (nreverse acc) 
                         (remove-duplicates addrs))
               (let ((tag (first (second (car list)))))
                 (case tag
                   (unresolve 
                    (destructuring-bind ((_ (__ addr)) . acc2) acc
                      (declare (ignore _ __))
                      (let ((pos (first (car list)))
                            (op (second (second (car list)))))
                        (recur (cdr list) 
                               (cons `(,pos (,op ,(symb "&" addr))) acc2)
                               (cons addr addrs)))))
                   (otherwise
                    (recur (cdr list) (cons (car list) acc) addrs)))))))
    (multiple-value-bind (bytecodes refered-addrs)
                         (recur bytecodes '() '())
      (sort 
       (append bytecodes
               (loop FOR addr IN refered-addrs
                     COLLECT `(,(- addr 0.5) ,(symb "&" addr))))
       #'<
       :key #'first))))

(defun convert-to-executable (bytecodes)
  (eval 
   `(body ,@(mapcar #'second (resolve-addrs bytecodes))
          (@pop %eax))))

(defun execute (filepath)
  (with-open-file (in filepath :element-type 'octet)
    (cl-asm:execute (convert-to-executable (read-bytecodes in))
                    (function int))))
