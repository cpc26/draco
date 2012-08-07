;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	sample Draco converter specifications
;;;
;;;
;;;	syntax:	(define-converter [ keyword... ])
;;;
;;;	keywords, mandantory:	:name
;;;				:input
;;;				:output
;;;				:syntax
;;;
;;;	keywords, optional:	:documentation
;;;				:package
;;;
;;;	notes:	Draco converter names must be unique.  if the specified
;;;		:name (symbol) is identical to the name of an existing
;;;		converter, the existing converter is redefined accordingly.
;;;
;;;		:input (symbol) and output (symbol) must be the names of
;;;		different file types.
;;;
;;;		:syntax (command) is a template which Draco uses to invoke
;;;		the converter.
;;;		
;;;		if :package (symbol) is not specified, the converter is
;;;		assumed to be a Lisp function.  otherwise, it names the
;;;		Draco package containing the converter.
;;;

(in-package draco)


(define-converter
	:name		FITS-to-OIF
	:documentation	"a FITS to OIF converter"
	:input		FITS
	:output		OIF
	:package	IRAF
	:syntax		"rfits ~in 1 ~out"
)


(define-converter
	:name		OIF-to-FITS
	:documentation	"an OIF to FITS converter"
	:input		OIF
	:output		FITS
	:package	IRAF
	:syntax		"wfits ~in 1 ~out"
)
