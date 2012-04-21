(in-package :pvm-bc)

(defun write-header (out header)
  ;; magic string
  (write-string-as-octets out *magic-string*)
  
  ;; header
  (write-int out (header-version header))
  (write-int out 0) ; constant-start: dummy
  (write-int out (header-symbol-count header))
  (write-int out 0) ; code-start: dummy
  (write-int out (header-code-size header))

  t
  )

(defun write-symbol-table (out symbol-table)
  (let ((acc '()))
    (maphash (lambda (symbol index)
               (push (cons index symbol) acc))
             symbol-table)
    (loop FOR (index . symbol) IN (sort acc #'< :key #'car)
          FOR s = (sb-ext:string-to-octets (symbol-name symbol))
          DO 
          ;; (write-byte 4 out) ; symbol
          (write-short out (length s))
          (write-sequence s out))))

(defun write-bc (out codes &key (symbol-table (make-hash-table)))
  (write-header out (header :symbol-count (hash-table-count symbol-table)
                            :code-size (length codes)))
  (let ((constant-start (file-position out)))
    (write-symbol-table out symbol-table)
    (let ((code-start (file-position out)))
      (write-sequence codes out)
      
      ;; constant-start, code-start
      (file-position out (+ 4 4))
      (write-int out constant-start)
      (file-position out (+ 4 4 4 4))
      (write-int out code-start)))
  t)


(defun write-bc-to-file (filepath codes &key (symbol-table (make-hash-table)))
  (with-open-file (out filepath 
                       :direction :output
                       :if-exists :supersede
                       :element-type 'octet)
    (write-bc out codes :symbol-table symbol-table)))

(defun w (codes)
  (write-bc-to-file "/tmp/test.bc" codes))
