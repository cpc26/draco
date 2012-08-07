;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(apsb-utilities);-*-
;;;
;;;	CLOS-related definitions
;;;

(in-package apsb-utilities)

#!(export '(class-subclasses))


(defmethod class-subclasses ((class class))
	"returns class' subclasses (not just direct subclasses)"
	(let ((class-direct-subclasses (class-direct-subclasses class)))
	     (aver (list class-direct-subclasses))
	     (nconc (copy-seq class-direct-subclasses)
		    (mapcan #'class-subclasses class-direct-subclasses))))
