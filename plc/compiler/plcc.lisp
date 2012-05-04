(in-package :plcc)

(defun hashmap-to-list (map &aux list)
  (maphash (lambda (k v)
             (push (cons k v) list))
           map)
  (mapcar #'car (sort list #'< :key #'cdr)))

(defun compile (exp)
  (let* ((*constants* (make-hash-table))
         (*quote* nil)
         (*tail* t)
         (rlt (compile-impl exp)))
    (make-bcobj :constants (hashmap-to-list *constants*)
                :code rlt)))

(defun output-to-file (bcobj out-file)
  (with-open-file (out out-file 
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (output-to-stream out bcobj)))

(defun compile-file (in-file out-file)
  (let* ((sexp (with-open-file (in in-file) (plcp:parse in)))
         (compiled (compile sexp)))
    (output-to-file compiled out-file)))

(defun compile-string (str out-file)
  (let* ((sexp (with-input-from-string (in str) (plcp:parse in))))
    (output-to-file (compile sexp) out-file)))
