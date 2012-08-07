;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	sample Draco primitive specifications
;;;
;;;
;;;	syntax:	(define-primitive [ keyword... ])
;;;
;;;	keywords, mandantory:	:name
;;;				:input
;;;				:output
;;;
;;;	keywords, optional:	:documentation
;;;				:primitives
;;;				:implementations
;;;
;;;	notes:	primitive names must be unique.  if the specified
;;;		:name (symbol) is identical to the name of an existing
;;;		primitive, the existing primitive is redefined accordingly.
;;;
;;;		:output must be a data-type name (symbol) or a sequence of
;;;		data-type names.  :input may take the same form, but in its
;;;		more general form, each input data-type name is qualified by
;;;		a :reconcile type, one of {nil, :conjunctive, :disjunctive}.
;;;		the default :reconcile type is nil.  see the specification
;;;		of the primitive CR-removal for an example of :input syntax.
;;;
;;;		if specified, :primitives must be a primitive name (symbol)
;;;		or a sequence of primitive names.
;;;
;;;		if specified, :implementations must be a implementation name
;;;		(symbol) or a sequence of implementation names.
;;;

(in-package draco)


(define-primitive
	:name			CR-removal
	:documentation		"remove cosmic ray hits"
	:reconciled-input	((image-set :disjunctive))
	:output			image
	:implementations 	(STSDAS-CR-removal)
)

(define-primitive
	:name			find-like-images
	:documentation		"find images that may be meaningfully combined"
	:reconciled-input	data-set
	:output			image-set
	:implementations 	(find-exposures)
)


(define-primitive
	:name			extract
	:documentation		"extract spectra"
	:reconciled-input	((data-set :disjunctive))
	:output			data-set
	:implementations 	(NOAO-extract)
)

(define-primitive
	:name			flatten
	:documentation		"flatten science images"
	:reconciled-input	((data-set :disjunctive))
	:output			data-set
	:implementations 	(NOAO-flatten)
)

(define-primitive
	:name			remove-bias
	:documentation		"remove bias signal"
	:reconciled-input	((data-set :disjunctive))
	:output			data-set
	:implementations 	(NOAO-remove-bias)
)

(define-primitive
	:name			remove-dark
	:documentation		"remove dark signal"
	:reconciled-input	((data-set :disjunctive))
	:output			data-set
	:implementations 	(NOAO-remove-dark)
)
