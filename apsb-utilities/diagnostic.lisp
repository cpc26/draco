;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(apsb-utilities);-*-
;;;
;;;	for diagnostics
;;;

(in-package apsb-utilities)

#!(export '(*diagnostic-stream*))


;; constant (duh!)
;; format string
(defconstant	*missing-keyword-format*
	"~&~A: missing :~A keyword argument~%"
	"arguments: function name, keyword name"
)


;; special variable
(defparameter	*diagnostic-stream*
	*standard-output*
	"diagnostic output stream."
)


;; declaration
(declaim (string *missing-keyword-format*))
(declaim (stream *diagnostic-stream*))
