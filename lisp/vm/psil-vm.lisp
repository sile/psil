(in-package :psil-vm)

(defun exec (source-spec &optional env)
  (etypecase source-spec
    (string (pvm.executor:execute-from-file source-spec env))
    (stream (pvm.executor:execute-from-stream source-spec env))
    (array  (pvm.executor:execute-from-octets source-spec env))))
