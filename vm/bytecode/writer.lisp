(in-package :pvm-bc)

(defun write-header (out header)
  ;; magic string
  (write-string-as-octets out *magic-string*)
  
  ;; header
  (write-int out (header-version header))
  (write-int out (header-code-size header))

  t
  )
