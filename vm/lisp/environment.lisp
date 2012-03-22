(in-package :pvm)

(defstruct byte-stream 
  (pos 0 :type fixnum)
  (bytes #() :type (array octet)))

(defstruct bcobj
  version
  id
  label-table
  symbol-table
  constant-table
  (code-stream t :type byte-stream))

(defstruct label-info
  (id 0 :type fixnum :read-only t)
  (pos -1 :type fixnum)) ; 実際のアドレスは読み込み時に解決される


