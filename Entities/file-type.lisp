;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	file-type definitions
;;;

(in-package draco)


;; constant (duh!)
;; format string
(defconstant	*file-type-cycle-format*
	"~&File type cycle (containing ~A) detected."
	"argument: file type"
)


;; variable
(defvar	*root-file-types*
	nil
	"file types that have no proper super types"
)


;; declarations
(declaim (string *file-type-cycle-format*))
(declaim (list *root-file-types*))

(declaim (function match-signatures (signature signature) boolean))
(declaim (function merge-signatures (&rest list) signature))


;; internal class-specific method definitions
(defmethod file-type-subtypep ((file-type1 file-type) (file-type2 file-type))
	"(find file-type2 (super-types file-type1))"
	(find file-type2 (find-super-types file-type1)))


(defmethod find-super-types ((file-type file-type))
	"computes and returns (super-types file-type)"
	(with-accessors ((super-type super-type)
			 (super-types super-types)) file-type
		(when (and super-type (null (cdr super-types)))
		      (setf super-types
			    (nconc super-types (find-super-types super-type))))
		super-types))


(defmethod shared-initialize :after
	   ((file-type file-type) slot-names &rest initargs)
	"set sub-types and super-types slots"
	(declare (ignore initargs slot-names))
	(with-accessors ((super-type super-type)
			 (super-types super-types)) file-type
		(when super-type
		      (pushnew file-type (sub-types super-type)))
		(unless super-types
			(setf super-types (list file-type))))
	(values))


(defmethod nearest-super-type ((file-type file-type) file-types)
	"
	returns the element of file-types that is file-type's closest
	ancestor in the super-type hierarchy; returns nil if no such
	element exists.  (optimistic implementation)"
	(aver (sequence file-types))

	(or (find file-type file-types)
	    (with-accessors ((super-type super-type)) file-type
		(and super-type
		     (nearest-super-type super-type file-types)))))


(defmethod signature ((file-data sequence))
	"return signature defined by file-data, a file-datum sequence"
	(typed-vector 'file-type
		(delete-duplicates
			(map '(vector file-type) #'file-type file-data))
		:inhibit-checking t))


;; internal function definitions
(defun match-signatures (input pattern)
	"returns nil only if input does not match pattern.  (could be faster)"
	(aver (signature input pattern))
	(let ((super-types (mapcar #'(lambda (file-type)
					(aver (file-type file-type))
					(nearest-super-type file-type pattern))
				   (coerce input 'list))))
	     (aver (list super-types))
	     (not (or (find nil super-types)
		      (has-duplicates super-types)))))


(defun merge-signatures (&rest args)
	"merges signature arguments into a single signature"
	(aver (list args))
	(delete-duplicates (apply #'concatenate '(vector file-type) args)))
