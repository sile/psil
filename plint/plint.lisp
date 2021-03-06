(in-package :plint)

(defun parse (input-stream)
  (plint.parser:parse input-stream))

(defun parse-string (str)
  (with-input-from-string (in str)
    (parse in)))

(defun parse-file (filepath)
  (with-open-file (in filepath)
    (parse in)))

(defun eval-ast (ast)  ; ast = result of parse function
  (plint.eval:eval-ast ast))

