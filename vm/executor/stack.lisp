(in-package :pvme)

(defstruct stack
  (top  0 :type fixnum)
  (base 0 :type fixnum)
  (data t :type simple-vector))

#|
[stack]
-- [top]
返り値
一つ前のtopへのリンク
一つ前のbaseへのリンク
リターンアドレス
-- [base]
x-y ローカル変数
n-x closes
0-n 引数

[conti]
stackの全コピー
|#

(defun create-stack (&optional (size 4096))
  (make-stack :data (make-array size)))

(defmethod print-object ((o stack) stream)
  (print-unreadable-object (o stream)
    (with-slots (top base data) o
      (format stream "~a ~a" (subseq data 0 base) (subseq data base top)))))

(defun spush (stack x)
  (with-slots (top data) (the stack stack)
    (setf (aref data top) x)
    (incf top))
  stack)

(defun spop (stack)
  (with-slots (top data) (the stack stack)
    (aref data (decf top))))

(defun local-ref (stack i)
  (with-slots (base data) (the stack stack)
    (aref data (- i 1))))

(defun local-set (stack i value)
  (with-slots (base data) (the stack stack)
    (setf (aref data (- i 1)) value)))

(defun sreserve (stack n)
  (incf (stack-top stack) n)
  stack)

(defun create-frame (stack return-address)
  (with-slots (top base data) (the stack stack)
    (let ((prev-top top)
          (prev-base base))
      (setf base top)
      (spush stack return-address)
      (spush stack prev-base)
      (spush stack prev-top)))
  stack)

;; => (values return-address return-value)
(defun destroy-frame (stack)
  (with-slots (top base data) (the stack stack)
    (let ((return-address (aref data base))
          (prev-top (aref data (+ base 1)))
          (prev-base (aref data (+ base 2)))
          (return-value (aref data (+ base 3))))
      (setf top prev-top
            base prev-base)
      (values return-address return-value))))
