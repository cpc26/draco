;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	sample Draco procedure specifications
;;;
;;;
;;;	syntax:	(define-procedure [ keyword... ])
;;;
;;;	keywords, mandantory:	:name
;;;				:primitives
;;;
;;;	keyword, optional:	:documentation
;;;
;;;	notes:	procedure names must be unique.  if the specified
;;;		:name (symbol) is identical to the name of an existing
;;;		procedure, the existing procedure is redefined accordingly.
;;;
;;;		:primitives must be a primitive name (symbol) or a sequence
;;;		of primitive names.
;;;

(in-package draco)


(define-procedure
	:name		extract-spectra
	:documentation	"remove bias/dark signals, flatten, then extract spectra"
	:primitives	(remove-bias remove-dark flatten extract)
)


(define-procedure
	:name		remove-CR-hits
	:documentation	"procedure for removing cosmic ray hits"
	:primitives	(find-like-images CR-removal)
)
