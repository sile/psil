(in-package :pvm)

(defun execute (in &key (initialize t))
  (pvme:execute-from-stream in :initialize initialize))

(defun execute-from-file (path &key (initialize t))
  (pvme:execute-from-file path :initialize initialize))

(defun write-bytecodes-to-file (filepath codes &key (symbol-table (make-hash-table)))
  (pvm-bc::write-bc-to-file filepath codes :symbol-table symbol-table))
