;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	GEIS (PODPS) calibration header file recognizer
;;;
;;;	all Draco file type recognizer functions must have this
;;;	syntax:  ( <recognizer name>  <file pathname> )
;;;	and must return a string describing the file type when
;;;	they succeed, or nil.
;;;

(in-package draco)


;; internal function declaration
(declaim (function GEIS-PODPS-calib-p (file-spec) (optional string)))


;; internal function definition
(defun GEIS-PODPS-calib-p (file-spec)
	"GEIS (PODPS) calibration header file recognizer"
	(aver (file-spec file-spec))

	;; if the file name ends with ".r[0-6]h" or ".b[2-6]h"
	;; and the file is a GEIS header file
	(let ((file-type (pathname-type file-spec)))
	     (aver ((optional string) file-type))
	     (and (= 4 (length file-type))
		  (eq #\h (schar file-type 2))
		  (or (and (eq #\r (schar file-type 0))
			   (find (schar file-type 1) "0123456"))
		      (and (eq #\b (schar file-type 0))
			   (find (schar file-type 1) "23456")))
		  (zerop (shell	(concatenate 'string
					(namestring *tools-dir*)
					"GEIS-p "
					(namestring file-spec))
				:output			#p"/dev/null"
				:error 			:output
				:if-output-exists	:append
				:ignore-status		t))
		  "GEIS (PODPS) calibration file")))
