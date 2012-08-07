;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	sample Draco implementation specifications
;;;
;;;
;;;	syntax:	(define-implementation [ keyword... ])
;;;
;;;	keywords, mandantory:	:name
;;;
;;;	keywords, optional:	:documentation
;;;				:package
;;;				:initialize
;;;				:initialize-once
;;;
;;;	notes:	implementation names must be unique.  if the specified
;;;		:name (symbol) is identical to the name of an existing
;;;		implementation, the existing implementation is redefined
;;;		accordingly.
;;;
;;;		the :name of an implementation is also the name of a
;;;		Common Lisp function that is responsible for putting the
;;;		appropriate commands into the data-dependent shell script
;;;		that is subsequently generated and invoked.  this function
;;;		must return the list of pathnames that will be created.
;;;
;;;		:package (symbol) names the Draco package used by the
;;;		function to implement the implementation.  if it is not
;;;		specified or specified as being nil, the eponymous Common
;;;		Lisp function is assumed to be "self-sufficient".
;;;
;;;		:input and :output must each be file-type names (symbol)
;;;		or sequences of file-type names.
;;;
;;;		if :package is non-nil, you may provide :initialize or
;;;		:initialize-once specifications (cons or string).  the
;;;		former specifies those commands that should be invoked
;;;		before each implementation invocation whereas the latter
;;;		specifies commands that only need to be invoked once.
;;;

(in-package draco)

; bogus definition!
(define-implementation
	:name			find-exposures
	:documentation		"find images that can be meaningfully combined"
	:package		nil	; Lisp function
	:input			directory
	:output			IRAF-image-list
)

; bogus definition!
(define-implementation
	:name			STSDAS-CR-removal
	:documentation		"STSDAS cosmic ray hits removal function"
	:package		IRAF
	:input			IRAF-image-list
	:output			GEIS-PODPS-calib-image
)

(define-implementation
	:name			NOAO-extract
	:documentation		"extract a directory's spectra"
	:package		IRAF
	:initialize-once	("noao" "imred" "ctioslit")
	:input			directory
	:output			directory
)

(define-implementation
	:name			NOAO-flatten
	:documentation		"flatten a directory's science images"
	:package		IRAF
	:initialize-once	("noao" "imred" "ccdred")
	:input			directory
	:output			directory
)

(define-implementation
	:name			NOAO-remove-bias
	:documentation		"remove bias signal from a directory's data"
	:package		IRAF
	:initialize-once	("noao" "imred" "ccdred")
	:input			directory
	:output			directory
)

(define-implementation
	:name			NOAO-remove-dark
	:documentation		"remove dark signal from a directory's data"
	:package		IRAF
	:initialize-once	("noao" "imred" "ccdred")
	:input			directory
	:output			directory
)
