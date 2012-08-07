;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	sample Draco data type specifications
;;;
;;;
;;;	syntax:	(define-data-type [ keyword... ])
;;;
;;;	keywords, mandantory:	:name
;;;				:file-types
;;;
;;;	keyword, optional:	:documentation
;;;
;;;	notes:	data type names must be unique.  if the specified
;;;		:name (symbol) is identical to the name of an existing
;;;		data type, the existing data type is redefined
;;;		accordingly.
;;;
;;;		:file-types must be a file-type name (symbol) or a
;;;		sequence of file-type names.
;;;

(in-package draco)


(define-data-type
	:name		image
	:documentation	"science image pixel array"
	:file-types	(GEIS-PODPS-calib-image OIF-object)
)

(define-data-type
	:name		image-set
	:documentation	"set (stack) of images"
	:file-types	(IRAF-image-list)
)
