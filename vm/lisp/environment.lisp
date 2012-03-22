(in-package :pvm)

(defstruct byte-stream 
  (pos 0 :type fixnum)
  (bytes #() :type (array octet)))

(defstruct bcobj
  version
  id
  (label-table 0 :type fixnum)
  (symbol-table 0 :type fixnum)
  (constant-table 0 :type fixnum)
  (code-stream t :type byte-stream))



