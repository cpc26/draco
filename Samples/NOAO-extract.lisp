;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	extract spectra.  implemented via the IRAF (NOAO) procedure apall.
;;;

(in-package draco)


;; constants (duh!)
(defconstant	*NOAO-apall-comment*
	"let us pray (that the default settings work)"
	"no arguments"
)

;; format string
(defconstant	*NOAO-apall-format*
	"apall ~a output=\"~a\" interactive=no edit=no fittrace=no extras=yes review=no nfind=1 background=\"fit\" weights=\"variance\" clean=yes"
	"string arguments: input image name, output spectrum name"
)


;; declaration
(declaim (string *NOAO-apall-comment* *NOAO-apall-format*))


;; method definition
(defmethod NOAO-extract ((stream stream) directory)
	"extract spectra from object images"
	(aver (file-spec directory))
	(add-comment stream *NOAO-apall-comment*)
	(let ((spectra nil))
	     (aver (list spectra))
	     (dolist (file-datum (file-data (get-instance 'file-type
						:name	'OIF-object)))
		     (aver (file-datum file-datum))
		     (push (namestring
				(make-pathname
					:defaults	(script-tempnam)
					:type		"imh"))
			   spectra)
		     (add-package-command stream
			(format nil *NOAO-apall-format*
				(file-name file-datum)
				(car spectra))))
	     spectra))
