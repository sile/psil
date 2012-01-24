(in-package :psil-vm)

(defun exec (source-spec)
  (etypecase source-spec
    (string (pvm.executor:execute-from-file source-spec))
    (stream (pvm.executor:execute-from-stream source-spec))
    (array  (pvm.executor:execute-from-octets source-spec))))
