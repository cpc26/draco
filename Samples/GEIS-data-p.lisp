;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	GEIS data file recognizer
;;;
;;;	all Draco file type recognizer functions must have this
;;;	syntax:  ( <recognizer name>  <file pathname> )
;;;	and must return a string describing the file type when
;;;	they succeed, or nil.
;;;

(in-package draco)


;; internal function declaration
(declaim (function GEIS-data-p (file-spec) (optional string)))


;; internal function definition
(defun GEIS-data-p (file-spec)
	"GEIS data file recognizer"
	(aver (file-spec file-spec))

	;; if the file name ends with a "d"
	;; and there is a corresponding GEIS header file
	(let ((file-type (pathname-type file-spec)))
	     (aver ((optional string) file-type))
	     (and file-type
		  (eq #\d (schar file-type (1- (length file-type))))
		  (zerop (shell	(format nil "~AGEIS-p ~A"
					*tools-dir*
					(make-pathname
					  :defaults	file-spec
					  :type		(substitute #\h #\d
							  file-type
							  :count	1
							  :from-end	t)))
				:output			#p"/dev/null"
				:error 			:output
				:if-output-exists	:append
				:ignore-status		t))
		  "GEIS data file (assumed)")))
