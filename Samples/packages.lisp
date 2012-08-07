;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	sample Draco package specifications
;;;
;;;
;;;	syntax:	(define-package [ keyword... ])
;;;
;;;	keywords, mandantory:	:name
;;;				:file-type
;;;				:invoke
;;;				:exit
;;;
;;;	keyword, optional:	:documentation
;;;				:pre-invoke
;;;				:initialize
;;;
;;;	notes:	Draco package names must be unique.  if the specified
;;;		:name (symbol) is identical to the name of an existing
;;;		data format, the existing Draco package is redefined
;;;		accordingly.
;;;
;;;		:file-type (symbol) must be the name of a Draco file
;;;		type.
;;;
;;;		:pre-invoke is an optional command (string) or sequence
;;;		of commands that must precede the actual invocation of
;;;		the package.  :invoke is the command (string) that
;;;		actually invokes the package.  :initialize is the
;;;		optional command (string) or sequence of commands that
;;;		initialize the package, and :exit is the command (string)
;;;		or command sequence for exiting the package.  :initialize
;;;		and :exit commands must use the package's command syntax.
;;;

(in-package draco)


(define-package
	:name		IRAF
	:documentation	"IRAF analysis package"
	:file-type	OIF
	:invoke		"/usr/stsci/irafx/unix/hlib/cl.csh"
	:initialize	("cl.logmode = \"errors trace\""
			 "cl.logfile = \"~log\""
			 "cl.keeplog = yes")
	:exit		"logout"
)
