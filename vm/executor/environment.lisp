(in-package :pvme)

(defstruct env
  (code-stream t :type octets-stream)
  (stack       t :type stack)
  (symbols     t :type symbol-table))
