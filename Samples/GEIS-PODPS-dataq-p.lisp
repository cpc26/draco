;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	GEIS (PODPS) data quality file recognizer
;;;
;;;	all Draco file type recognizer functions must have this
;;;	syntax:  ( <recognizer name>  <file pathname> )
;;;	and must return a string describing the file type when
;;;	they succeed, or nil.
;;;

(in-package draco)


;; internal function declaration
(declaim (function GEIS-PODPS-dataq-p (file-spec) (optional string)))


;; internal function definition
(defun GEIS-PODPS-dataq-p (file-spec)
	"GEIS (PODPS) data quality file recognizer"
	(aver (file-spec file-spec))

	;; if the file name ends with ".c1h", ".q0h", or ".q1h"
	;; and the file is a GEIS (header) file
	(let ((file-type (pathname-type file-spec)))
	     (aver ((optional string) file-type))
	     (and (string-equal "c1h" file-type)
		  (string-equal "q0h" file-type)
		  (string-equal "q1h" file-type)
		  (zerop (shell	(concatenate 'string
					(namestring *tools-dir*)
					"GEIS-p "
					(namestring file-spec))
				:output			#p"/dev/null"
				:error 			:output
				:if-output-exists	:append
				:ignore-status		t))
		  "GEIS (PODPS) data quality file")))
