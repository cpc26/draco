;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	print-related-object methods
;;;

(in-package draco)


;; constants (duh!)
;; format strings
(defconstant	*bad-procedure-format*
	"~%Procedure ~A has a nonempty ~(~A~) slot value."
	"argument: procedural primitive name, slot name"
)
(defconstant	*readable-object-body-format*
	"~{~%  :~(~A~)~24T~S~}~%)~%"
	"argument: slot initarg-value list"
)
(defconstant	*readable-object-header-format*
	"~%(define-~(~A~)"
	"argument: class name"
)


;; declaration
(declaim (string *bad-procedure-format* *readable-object-body-format*
		 *readable-object-header-format*))


;; the primary methods just print the opening parenthesis and the
;; appropriate macro name.  the :after method prints slot information
;; and the closing parenthesis, i.e. does most of the work.

#|
(defmethod print-readable-object :before ((entity entity) (stream stream))
	"make sure the instance is named"
	(unless (slot-boundp entity 'name)
		(setf (name entity)
		      (gentemp (class-name (class-of entity)) 'Draco))))
|#

(defmethod print-readable-object :before
	   ((primitive primitive) (stream stream))
	"inhibit printing of unused procedural primitive slots (ugh!)"
	(with-accessors ((procedure procedure)) primitive
		(when procedure
		      (dolist (slot-name '(reconciled-input output
					   implementations))
			      (aver (moniker slot-name))
			      (when (slot-boundp primitive slot-name)
				    (unless (zerop
						(length
						    (slot-value
							primitive slot-name)))
					    (draco-error *bad-procedure-format*
							 primitive slot-name)))
			      (slot-makunbound primitive slot-name)))))

(defmethod print-readable-object ((entity entity) (stream stream))
	"(define-<entity type> ...) by default"
	(format stream *readable-object-header-format*
		(class-name (class-of entity)))
	(values))

(defmethod print-readable-object ((draco-package draco-package) (stream stream))
	"try to hide CL's package type from the user"
	(format stream *readable-object-header-format* "package")
	(values))

(defmethod print-readable-object ((primitive primitive) (stream stream))
	"differentiates between procedural and non-procedural primitives"
	(format stream *readable-object-header-format*
		(if (procedure primitive)
		    (then "procedure")
		    (else "primitive")))
	(values))

;; local function definition
(labels
  ((pretty-slot-value (value)
	"return a slot value's printed representation"
	(aver ((or entity number pathname sequence symbol) value))
	(etypecase value
	  (number		value)
	  (string		value)
	  (symbol		(intern (string value)))
	  (pathname		(namestring value))
	  (vector		(map 'list #'pretty-slot-value value))
	  (list			(mapcar #'pretty-slot-value (true-list value)))
	  (imported-entity	(unless (slot-boundp value 'name)
					(setf (name value)
					      (gentemp
						(class-name (class-of value))
						'Draco)))
				(pretty-slot-value (name value)))
	  (entity		value)	; good luck, you'll need it!
	)))

  ;; local function declaration
  (declare (function pretty-slot-value
		     ((or entity number pathname sequence symbol))
		     (or entity list number string symbol)))

  (defmethod print-readable-object :after ((entity entity) (stream stream))
	"print slot initargs and values, and closing parenthesis"
	(let ((class (class-of entity))
	      (slots nil))
	     (aver (draco-class class)
		   (list slots))
	     (dolist (slot (nreverse (class-slots class)))
		     (unless (find (slot-definition-name slot)
				   (ephemeral-slots class))
			     (when (and (slot-boundp
					  entity (slot-definition-name slot))
					(slot-definition-initargs slot)
					(slot-definition-readers slot))
				   (push slot slots))))
	     (format stream *readable-object-body-format*
		(mapcan #'list
		    (mapcar #'car
			    (mapcar #'slot-definition-initargs slots))
		    (mapcar #'pretty-slot-value
			    (mapcar #'(lambda (readers)
					      (aver (list readers))
					      (funcall (car readers) entity))
				    (mapcar #'slot-definition-readers
					    slots))))))
	(values))
) ; labels
