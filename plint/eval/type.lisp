(in-package :plint.eval)

(defstruct type.number 
  value)

(defmethod print-object ((o type.number) out)
  (with-slots (value) o
    (format out "!~a" value)))

(defstruct type.string
  value)

(defmethod print-object ((o type.string) out)
  (with-slots (value) o
    (format out "!~a" value)))

(defstruct type.character
  value)

(defmethod print-object ((o type.character) out)
  (with-slots (value) o
    (format out "!~a" value)))

(defstruct type.boolean
  value)

(defmethod print-object ((o type.boolean) out)
  (with-slots (value) o
    (format out "!<~a>" (if value "TRUE" "FALSE"))))

(defstruct type.symbol
  name
  value
  parent)

(defmethod print-object ((o type.symbol) out)
  (with-slots (name) o
    (format out "!~a" name)))

(defstruct type.cons
  car
  cdr)

(defmethod print-object ((o type.cons) out)
  (with-slots (car cdr) o
    (format out "!(~a . ~a)" car cdr)))

(defstruct type.lambda
  params ; list of type.symbol
  body   ; ast
  env)
(defmethod print-object ((o type.lambda) out)
  (format out "!<LAMBDA>"))

(defstruct type.macro
  )
(defmethod print-object ((o type.macro) out)
  (format out "!MACRO"))

(defstruct type.special
  name
  native-fn)

(defmethod print-object ((o type.special) out)
  (with-slots (name) o
    (format out "!<SPECIAL ~a>" name)))

(defstruct type.undef)
(defmethod print-object ((o type.undef) out)
  (format out "!<UNDEF>"))
  
