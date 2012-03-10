#|
  S式形式のアセンブリ言語を、PVM用のバイトコードにコンパイル(アセンブル)する
|#
(defpackage pvm-compile
  (:use :common-lisp)
  (:nicknames :pvmc)
  (:export compile-to-file))
(in-package :pvm-compile)

;; 利用可能な操作(命令)のリスト
(defparameter *op-list*
  '((1 :int)
    (2 :add)
    (3 :sub)
    (4 :mul)
    (5 :div)
    (6 :mod)
    (7 :eql)
    (8 :less)

    (9 :dup)
    (10 :drop)
    (11 :swap)
    (12 :over)
    (13 :rot)

    (14 :rpush)
    (15 :rpop)
    (16 :rcopy)
    
    (17 :jump)
    (18 :jump-if)
    (19 :call)
    (20 :return)))

;; 数値をリトルエンディアンのバイト列に変換する
;; n -> '(b1 b2 b3 b4)
(defun int-to-bytes (n)
  (loop FOR i FROM 0 BELOW 4
        COLLECT (ldb (byte 8 (* i 8)) n)))

;; 操作名に対するコード値を取得する
(defun opcode (op)
  (assert #1=(find op *op-list* :key #'second))
  (first #1#))

;; コンパイル
(defun compile-to-bytecodes (codes)
  (loop WITH unresolves = '()  ; 未解決のアドレス参照
        WITH labels = '()      ; ラベルとアドレスのマッピング
        FOR code IN codes
        FOR pos = (length tmps)
    APPEND
    (etypecase code
      (integer `(,(opcode :int) ,@(int-to-bytes code))) ; 整数値構築
      (keyword (list (opcode code)))                    ; 一般的な操作
      (symbol (push `(,code ,pos) labels)               ; アドレス(PC)参照用のラベル
              '())
      (cons (ecase (first code)                         ; アドレス参照
              (:addr (push `(,(second code) ,(1+ pos)) unresolves)
                     `(,(opcode :int) 0 0 0 0))))) ; この時点では実際のアドレスが不明なので 0 を設定しておく
    INTO tmps
    FINALLY
    (let ((bytecodes (coerce tmps 'vector)))
      ;; アドレス解決
      (loop FOR (label offset) IN unresolves
            FOR label-addr = (second (assoc label labels))
        DO
        (setf (subseq bytecodes offset (+ offset 4)) (int-to-bytes label-addr)))

      (return bytecodes))))

;; コンパイルして結果をファイルに出力する
(defun compile-to-file (filepath assembly-codes)
  (let ((bytecodes (compile-to-bytecodes assembly-codes)))
    (with-open-file (out filepath :direction :output
                                  :if-exists :supersede
                                  :element-type '(unsigned-byte 8))
      (write-sequence bytecodes out)))
  t)
