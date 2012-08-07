;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	Draco diagnostic definitions
;;;

(in-package draco)


;; constant (duh!)
;; format string
(defconstant	*inform-prefix-format*
	"~&Draco: "
	"printed before each informational message"
)
(defconstant	*missing-keyword-format*
	"Draco:~A: missing :~A keyword argument~%"
	"arguments: function name, keyword name"
)


;; declarations
(declaim (string *inform-prefix-format* *missing-keyword-format*))

(declaim (function draco-error (string &rest list)))
(declaim (function draco-inform (string &rest list)))


;; internal function definition
(defun draco-error (format-string &rest format-args)
	"Draco error reporting/recovery function"
	(aver (string format-string)
	      (list format-args))
	(apply #'error format-string format-args)
	;; (values)
)


(defun draco-inform (format-string &rest format-args)
	"Draco information function"
	(aver (string format-string)
	      (list format-args))
	(format *error-output* *inform-prefix-format*)
	(apply #'format *error-output* format-string format-args)
	(terpri *error-output*)
	(values)
)
