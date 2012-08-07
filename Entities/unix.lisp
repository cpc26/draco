;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	built-in Unix definitions
;;;

(in-package draco)


;; constants (duh!)
;; format string
(defconstant	*Unix-redefinition-format*
	"~&Draco:get-instance: attempt to redefine ~A."
	"argument: Unix imported-entity instance"
)


;; declarations
(declaim (string *Unix-redefinition-format*))


(define-internal-file-type
	:name		Unix
	:documentation	"Unix file type (built-in)"
	:recognizer	(make-pathname	:defaults	*tools-dir*
					:name		"draco-file"
					:type		"sh")
)

(define-internal-file-type
	:name		directory
	:documentation	"Unix directory"
	:super-type	Unix
	:recognizer	(make-pathname	:defaults	*tools-dir*
					:name		"directoryp"
					:type		"sh")
)


(defmethod reinitialize-instance :before
	   ((imported-entity imported-entity) &rest initargs &key name)
	"protect Unix imported-entity redefinition"
	(aver (list initargs)
	      (symbol name))
	;; almost correct; a nop call is innocuous,
	;; but we only check for the simplest nop's.
	(when (and (eql name 'Unix)
		   (> (length initargs) 2))
	      (draco-error *Unix-redefinition-format* imported-entity)))
