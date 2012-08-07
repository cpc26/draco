;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	Draco installation-dependent definitions
;;;

(in-package draco)


;; constants (duh!)
(defconstant	*default-reporter*
	'write-default-report
	"default inventory reporter"
)
(defconstant	*default-reporter-type*
	:f-sorted
	"default inventory reporter type"
)
(defconstant	*tools-dir*
	(make-pathname
	    :directory	(namestring
			    (translate-logical-pathname #p"Draco:Tools")))
	"Draco installation tools directory"
)


;; special variables
(defvar		*init-file-name*
	".Draco"
	"Draco initialization file name"
)
(defvar		*init-file-type*
	"lisp"
	"Draco initialization file type"
)
(defvar		*inventory-name*
	"Draco"
	"default Draco inventory file name"
)
(defvar		*inventory-type*
	"inv"
	"default Draco inventory file type"
)
(defvar		*log-file-type*
	"log"
	"Draco log file type"
)
(defvar		*script-name*
	"Draco"
	"default Draco script name"
)
(defvar		*script-type*
	"sh"
	"default Draco script type"
)


;; declarations
(declaim (pathname *tools-dir*))
(declaim (string *init-file-name* *init-file-type*
		 *inventory-name* *inventory-type*
		 *log-file-type*
		 *script-name* *script-type*))
(declaim (symbol *default-reporter* *default-reporter-type*))
