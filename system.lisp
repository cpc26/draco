;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	basic definitions
;;;

(in-package draco)


;; constants (duh!)
(defconstant	*Draco-package*
	(find-package 'Draco)
	"Draco's CL package"
)
(defconstant	*version*
	"1"
	"Draco version string"
)


;; declarations
(declaim (package *Draco-package*))
(declaim (string *version*))
