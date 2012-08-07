;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	entity data type (and related) definitions
;;;

(in-package draco)


;; constant (duh!)
;; format string
(defconstant	*bad-definition-format*
	"~&illegal redefinition of ~A"
	"argument: entity"
)

;; declaration
(declaim (string *bad-definition-format*))


;;; the meta-class Draco-class enables us to find all instances of any class.

(defclass Draco-class (standard-class)
	(
	 (ephemeral-slots
		:type		list
		:documentation	"slots print-readable-object should ignore"
		:initform	nil
		:accessor	ephemeral-slots-1)
	 (instances
		:type		list
		:documentation	"entity instance list"
		:initform	nil 
		:accessor	instances)
	)
	(:documentation "Draco meta-class")
)

(defmethod ephemeral-slots ((class-name symbol))
	"return ephemeral-slot names, including those of the parent class"
	(ephemeral-slots (find-class class-name)))

(defmethod ephemeral-slots ((draco-class draco-class))
	"return ephemeral-slot names, including those of the parent class"
        (append (and (not (eq draco-class (find-class 'entity)))
		     (mapcan #'ephemeral-slots
			     (class-direct-superclasses draco-class)))
		(copy-seq (ephemeral-slots-1 draco-class))))

(defmethod (setf ephemeral-slots) (new-epehemeral-slots (class-name symbol))
	"set specified ephemeral-slots list"
	(setf (ephemeral-slots-1 (find-class class-name))
	      new-epehemeral-slots))

(defmethod make-instance :around ((draco-class draco-class) &rest initargs)
	"maintain class instance lists"
	(aver (list initargs))
	(let ((instance (call-next-method)))
	     (aver (entity instance))
	     (push instance (instances draco-class))
	     instance))


(defclass entity (standard-object)
	(
	 (status
		:type		status-type
		:documentation	"see (describe 'status-type)"
		:initform	:referenced
		:initarg	:status
		:accessor	status)
	)
	(:documentation "Draco entity class")
	(:metaclass Draco-class)
)

(setf (ephemeral-slots 'entity) '(status))

(deftype status-type ()
	"
	indicates an imported entity's status.
		:referenced -> entity was referenced but not initialized
		:initialized -> entity is initialized but not validated
		:validating -> entity is being validated
		:validated -> entity is well-formed
		:obsolete -> entity was initialized, but is now obsolete"
	'(member :referenced :initialized :validating :validated :obsolete)
)

(defmethod class-slots ((draco-class draco-class))
	"weed out CLOS' slots"
	(append (and (not (eq draco-class (find-class 'entity)))
		     (mapcan #'class-slots
			     (class-direct-superclasses draco-class)))
		(copy-seq (class-direct-slots draco-class))))

(defmethod initialize-entity ((entity entity))
		"entities require no further initialization by default"
		(values))

(defmethod reinitialize-instance :before ((entity entity) &rest initargs)
	"prevent illegal redefinition"
	(aver (list initargs))
	(case (status entity)
	  (:referenced	)
	  (:obsolete	)
	  (t		(dolist (slot (class-slots (class-of entity)))
				(let* ((name (slot-definition-name slot))
				       (default (gentemp name))
				       (inits (slot-definition-initargs slot))
				       (value (getf initargs name default)))
				      (aver (moniker name default)
					    (list inits)
					    (t value))
				      (when (and inits
						 (slot-boundp entity name))
					    (unless (or (eql value default)
							(eql value
							     (slot-value
								entity name)))
						    (draco-error
							*bad-definition-format*
							entity))))))
	))

(defmethod shared-initialize :after ((entity entity) slot-names &rest initargs)
	"set :status slot to :initialized when appropriate"
	(declare (ignore slot-names))
	(aver (list initargs))
	(when (> (length initargs) 2)	; :name buys us 2 args
	      (setf (status entity) :initialized)))


(defclass imported-entity (entity)
	(
	 (name
		:type		moniker
		:documentation	"imported entity name"
		:initarg	:name
		:accessor	name)
	 (documentation
		:type		(optional string)
		:documentation	"imported entity description"
		:initform	nil
		:initarg	:documentation
		:reader		doc-string)
	 (defined-by
		:type		list
		:documentation	"imported entity definition location"
		:initform	nil	; initialized by fire-load method
		:accessor	defined-by)
	)
	(:documentation "Draco imported (public) entity class")
	(:metaclass Draco-class)
)

(setf (ephemeral-slots 'imported-entity) '(defined-by))

(defmethod print-object ((imported-entity imported-entity) (stream stream))
	(print-unreadable-object (imported-entity stream :type t :identity t)
		(princ (or (doc-string imported-entity)
			   (and (slot-boundp imported-entity 'name)
				(name imported-entity))
			   "Odysseus")
		       stream)))


(defclass converter (imported-entity)
	(
	 (input
		:type		file-type
		:documentation	"source file-type"
		:initarg	:input
		:reader		input)
	 (output
		:type		file-type
		:documentation	"destination file-type"
		:initarg	:output
		:reader		output)
	 (draco-package
		:type		(optional draco-package)
		:documentation	"Draco package containing converter"
		:initform	nil
		:initarg	:draco-package
		:reader		draco-package)
	 (syntax
		:type		(vector string)
		:documentation	"converter invocation template"
		:initarg	:syntax
		:reader		syntax)
	 (_syntax
		:type		commands
		:documentation	"converter invocation template (internal)"
		:initarg	:_syntax
		:accessor	_syntax)
	)
	(:documentation	"Draco's (file type) converter class")
	(:metaclass Draco-class)
)

(setf (ephemeral-slots 'converter) '(_syntax))


(defclass data-type (imported-entity)
	(
	 (file-types
		:type		signature
		:documentation	"data-type's corresponding file-types"
		:initarg	:file-types
		:reader		file-types)
	)
	(:documentation	"Draco's data type class")
	(:metaclass Draco-class)
)

(defmethod shared-initialize :after
	   ((data-type data-type) slot-names &rest initargs)
	"(re)initialize the associated file-type's data-type slots"
	(declare (ignore slot-names))
	(aver (list initargs))
	(when (find :file-types initargs)
	      (map nil
		   #'(lambda (file-type)
			     (aver (file-type file-type))
			     (setf (data-type file-type) data-type))
		   (file-types data-type))))


(defclass draco-package (imported-entity)
	(
	 (file-type
		:type		(optional file-type)
		:documentation	"package's native file-type"
		:initform	nil
		:initarg	:file-type
		:reader		file-type)
	 (pre-invoke
		:type		(vector string)
		:documentation	"package pre-invocation (set-up) command(s)"
		:initarg	:pre-invoke
		:reader		pre-invoke)
	 (_pre-invoke
		:type		commands
		:documentation	"package pre-invocation command(s) (internal)"
		:initarg	:_pre-invoke
		:accessor	_pre-invoke)
	 (invoke
		:type		string
		:documentation	"package invocation command"
		:initarg	:invoke
		:reader		invoke)
	 (_invoke
		:type		command
		:documentation	"package invocation command (internal)"
		:initarg	:_invoke
		:accessor	_invoke)
	 (initialize
		:type		(vector string)
		:documentation	"package initialization command(s)"
		:initarg	:initialize
		:reader		initialize)
	 (_initialize
		:type		commands
		:documentation	"package initialization command(s) (internal)"
		:initarg	:_initialize
		:accessor	_initialize)
	 (exit
		:type		(vector string)
		:documentation	"package exit command(s)"
		:initarg	:exit
		:reader		exit)
	 (_exit
		:type		commands
		:documentation	"package exit command(s) (internal)"
		:initarg	:_exit
		:accessor	_exit)
	)
	(:documentation	"Draco's (analysis) package class")
	(:metaclass Draco-class)
)

(setf (ephemeral-slots 'draco-package)
      '(_pre-invoke _invoke _initialize _exit))


(defclass file-type (imported-entity)
	(
	 (recognizer
		:type		(or function pathname)
		:documentation	"CL function or shell command"
		:initarg	:recognizer
		:reader		recognizer)
	 (super-type
		:type		(optional file-type)
		:documentation	"file-type's super-type"
		:initform	nil
		:initarg	:super-type
		:reader		super-type)
	 (reporter
		:type		(or function KV-reporter)
		:documentation	"KV-reporter or CL function"
		:initarg	:reporter
		:reader		reporter)
	 (reporter-type
		:type		reporter-type
		:documentation	"file-type reporter's reporter-type"
		:initarg	:reporter-type
		:reader		reporter-type)
	 (file-data
		:type		list	; because it's built up incrementally
		:documentation	"data for files of this type"
		:initform	nil
		:accessor	file-data)
	 (data-type
		:type		(optional data-type)
		:documentation	"data type associated with this file type"
		:initform	nil
		:accessor	data-type)
	 (sub-types
		:type		list	; because it's built up incrementally
		:documentation	"file-type's immediate sub-types"
		:initform	nil
		:accessor	sub-types)
	 (super-types
		:type		cons	; because it's built up incrementally
		:documentation	"file-type's super-type, its super-type, etc."
		:initform	nil
		:accessor	super-types)
	)
	(:documentation	"Draco's file type class")
	(:metaclass Draco-class)
)

(setf (ephemeral-slots 'file-type) '(file-data))

(deftype reporter-type ()
	"
	indicates a file-type (KV) reporter's behavior.
		nil	  -> reporter is nil;
		:c-sorted -> sort entries by their contents;
		:f-sorted -> sort entries by file name
	default is *default-reporter-type*."
	'(member nil :c-sorted :f-sorted)
)

;; a "signature" describes the file-type's associated with a single
;; implementation or a procedural primitive's implementation vector.

(deftype signature ()
	"(vector file-type)"
	'(and (vector file-type) (not (satisfies has-duplicates)))
)


(defclass implementation (imported-entity)
	(
	 (draco-package
		:type		(optional draco-package)
		:documentation	"package containing implementation"
		:initform	nil
		:initarg	:draco-package
		:reader		draco-package)
	 (input
		:type		signature
		:documentation	"implementation input file-type"
		:initarg	:input
		:reader		input)
	 (output
		:type		signature
		:documentation	"implementation output file-type"
		:initarg	:output
		:reader		output)
	 (initialize
		:type		(vector string)
		:documentation	"every-time initialization command(s)"
		:initarg	:initialize
		:reader		initialize)
	 (_initialize
		:type		commands
		:documentation	"every-time init. command(s) (internal)"
		:initarg	:_initialize
		:accessor	_initialize)
	 (initialize-once
		:type		(vector string)
		:documentation	"one-time-only initialization command(s)"
		:initarg	:initialize-once
		:reader		initialize-once)
	 (_initialize-once
		:type		commands
		:documentation	"one-time-only init. command(s) (internal)"
		:initarg	:_initialize-once
		:accessor	_initialize-once)
	 (impl-fn
		:type		function-spec
		:documentation	"Common Lisp function that does all the work"
		:initarg	:impl-fn
		:reader		impl-fn)
	 (primitive
		:type		(optional primitive)
		:documentation	"primitive associated with this implementation"
		:initform	nil
		:accessor	primitive)
	)
	(:documentation	"Draco's implementation class")
	(:metaclass Draco-class)
)

(setf (ephemeral-slots 'implementation)
      '(_initialize _initialize-once _syntax))

(deftype impl-vector ()
	"(vector implementation)"
	'(vector implementation)
)

(defmethod shared-initialize :after
	   ((implementation implementation) slot-names &rest initargs)
	"(re)initialize the implementation's internal command slots"
	(declare (ignore slot-names))
	(aver (list initargs))
	(with-accessors ((input input)
			 (output output)
			 (initialize initialize) (_initialize _initialize)
			 (initialize-once initialize-once)
				(_initialize-once _initialize-once)
			 (syntax syntax) (_syntax _syntax)) implementation
		(when (find :initialize initargs)
		      (setf _initialize
			    (translate-cmd-strings initialize
				input output implementation)))
		(when (find :initialize-once initargs)
		      (setf _initialize-once
			    (translate-cmd-strings initialize-once
				input output implementation)))
		(when (find :syntax initargs)
		      (setf _syntax
			    (translate-cmd-strings syntax
				input output implementation)))))


(defclass initialization-file (fire imported-entity)
	(
	 (load-immediately
		:type		boolean
		:documentation	"non-nil -> load asap; nil -> load as needed"
		:initform	nil
		:initarg	:load-immediately
		:accessor	load-immediately)
	)
	(:documentation	"Draco's data file class")
	(:metaclass Draco-class)
)


(defclass KV-reporter (imported-entity)
	(
	 (lex-function
		:type		(or function pathname)
		:documentation	"CL function or shell command"
		:initarg	:lex-function
		:reader		lex-function)
	 (reporter-type
		:type		(and reporter-type (not null))
		:documentation	":c-sorted or :f-sorted (default)"
		:initform	:f-sorted
		:initarg	:reporter-type
		:reader		reporter-type)
	 (report-keywords
		:type		list
		:documentation	"desired keyword list"
		:initarg	:report-keywords
		:accessor	report-keywords)
	 (reporter
		:type		compiled-function
		:documentation	"behavior is defined by other slot values"
		:accessor	reporter)
	)
	(:documentation	"Draco's KV-reporter class")
	(:metaclass Draco-class)
)


(flet
  ((reconciled-input-p (arg)
	"returns non-nil only if arg is a valid reconciled-input list"
	(aver (t arg))
	(and (listp arg)
	     (not (has-duplicates (mapcar #'car arg)))
	     (every #'(lambda (reconcile-type)
			      (aver (t reconcile-type))
			      (typep reconcile-type 'reconcile-type))
		    (mapcar #'cdr arg)))))

  ;; local function declaration
  (declare (function reconciled-input-p (list) boolean))

  (defclass primitive (imported-entity)
	(
	 (procedure
		:type		boolean
		:documentation	"primitive is procedural only if non-nil"
		:initform	nil
		:initarg	:procedure
		:reader		procedure)
	 (reconciled-input
		:type		(satisfies reconciled-input-p)
		:documentation	"maps input data-type's to reconcile-type's"
		:initarg	:reconciled-input
		:reader		reconciled-input)
	 (output
		:type		(and (vector data-type)
				     (not (satisfies has-duplicates)))
		:documentation	"primitive output data-type"
		:initarg	:output
		:reader		output)
	 (primitives
		:type		(optional (vector primitive))
		:documentation	"procedural primitive's primitive list"
		:initform	nil
		:initarg	:primitives
		:reader		primitives)
	 (implementations
		:type		(optional impl-vector)
		:documentation	"non-procedural primitive's implementation list"
		:initform	nil
		:initarg	:implementations
		:reader		implementations)
	 (impl-map
		:type		list	; alist
		:documentation	"primitive's implementation map"
		:accessor	impl-map)
	)
	(:documentation	"Draco's primitive class")
	(:metaclass Draco-class)
  ) ; defclass
) ; flet

;;; if the reconcile vector contains several :disjunctive entries, we will
;;; assume (for now) that the proper course of action is to disjunctive over
;;; these data types in parallel, i.e. invoke the primitive on the first set
;;; of disjunctive data types, then on the second, and so forth.

(deftype reconcile-type ()
	"
	indicates a primitive's behavior when it encounters multiple inputs
	for a given data-type.
		nil -> error;
		:conjunctive -> process all simultaneously;
		:disjunctive -> process each individually
	default is nil."
	'(member nil :conjunctive :disjunctive)
)


(defclass internal-entity (entity)
	()
	(:documentation "Draco internal (private) entity class")
	(:metaclass Draco-class)
)


(defclass constraint (internal-entity)
	(
	 (name
		:type		moniker
		:documentation	"constraint name"
		:initarg	:name
		:accessor	name)
	 (documentation
		:type		(optional string)
		:documentation	"constraint description"
		:initform	nil
		:initarg	:documentation
		:reader		doc-string)
	 (constraint
		:type		cons
		:documentation	"this form must not signal an error"
		:initarg	:constraint
		:reader		constraint)
	)
	(:documentation	"Draco's constraint class")
	(:metaclass Draco-class)
)


(defclass file-datum (internal-entity)
	(
	 (file-name
		:type		string
		:documentation	"name of science data file"
		:initarg	:file-name
		:accessor	file-name)
	 (file-type
		:type		(optional file-type)
		:documentation	"file type of science data file"
		:initform	nil
		:initarg	:file-type
		:reader		file-type)
	 (KV-pairs
		:type		list
		:documentation	"science data file KV attributes"
		:initform	nil
		:initarg	:KV-pairs
		:accessor	KV-pairs)
	 (description
		:type		string
		:documentation	"description (recognizer output) of data file"
		:initform	""
		:initarg	:description
		:accessor	description)
	)
	(:documentation	"Draco's file datum class")
	(:metaclass Draco-class)
)

(defmethod print-object ((file-datum file-datum) (stream stream))
	(print-unreadable-object (file-datum stream :type t :identity t)
		(format stream "~a (~a)"
			(file-name file-datum) (file-type file-datum))))


(defclass SIV (internal-entity)
	(
	 (impl-vector
		:type		impl-vector
		:documentation	"implementation vector for some primitive"
		:initarg	:impl-vector
		:reader		impl-vector)	
	 (signature
		:type		signature
		:documentation	"impl-vector slot value signature"
		:initarg	:signature
		:reader		signature)	
	)
	(:documentation	"Draco's signed implementation vector class")
	(:metaclass Draco-class)
)


(defclass user-data (internal-entity)
	(
	 (file-type
		:type		file-type
		:documentation	"file type of files slot value"
		:initarg	:file-type
		:reader		file-type)	
	 (input
		:type		boolean
		:documentation	"t -> data files are input files"
		:initarg	:input
		:reader		input)	
	 (implementation
		:type		(optional implementation)
		:documentation	"for input data files only"
		:initform	nil
		:initarg	:implementation
		:reader		implementation)	
	 (reconcile
		:type		reconcile-type
		:documentation	"for input data files only"
		:initarg	:reconcile
		:accessor	reconcile)	
	 (files
		:type		list
		:documentation	"list of data files of type file-type"
		:accessor	files)	
	 (number-of
		:type		fixnum
		:documentation	"number of data files of type file-type"
		:accessor	number-of)	
	)
	(:documentation	"Draco's user data class")
	(:metaclass Draco-class)
)


;;; commands start out as strings which may contain tilde directives,
;;; e.g. ~log or ~in.  when an instance is validated, these directives
;;; are replaced by the corresponding command-element's, e.g. :log or
;;; a user-data instance.  at this point, the command is represented
;;; as a vector of data-independent command-element's.  the user-data
;;; instances are populated with information about the user's data at
;;; script-generation time, i.e. the vector becomes data-dependent.
;;; then, the appropriate strings are substituted for :log and the
;;; user-data instances and the resulting string vector is transformed
;;; into a data-dependent string (by concatenating the vector elements).

(deftype command ()
	"a command is either a string or a vector of command elements"
	'(or string (vector command-element))
)

(deftype command-element ()
	"a command element is either a user-data instance, a string, or :log"
	'(or string user-data (eql :log))
)

(deftype commands ()
	"several commands may be needed to implement a single abstract command"
	'(vector command)
)
