(in-package :pvm-bc)

(defun write-header (out header)
  ;; magic string
  (write-string-as-octets out *magic-string*)
  
  ;; header
  (write-int out (header-version header))
  (write-int out (header-code-size header))

  t
  )

(defun write-bc (out codes)
  (write-header out (header :code-size (length codes)))
  (write-sequence codes out)
  t)

(defun write-bc-to-file (filepath codes)
  (with-open-file (out filepath 
                       :direction :output
                       :if-exists :supersede
                       :element-type 'octet)
    (write-bc out codes)))
