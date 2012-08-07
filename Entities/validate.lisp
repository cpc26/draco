;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	validation methods (enforce intra-instance constraints)
;;;

(in-package draco)


;; constants (duh!)
;; format strings
(defconstant	*bad-reporter-type-format*
	"~&The reporter type of ~A does not match its reporter's reporter type"
	"argument: file type name"
)
(defconstant	*mismatched-io-format*
	"~&The ~(~A~) list of ~A does not match the corresponding list in ~A."
	"argument: i/o slot name, primitive name, implementation name"
)
(defconstant	*missing-file-format*
	"~&File ~A cannot be read."
	"argument: pathname"
)
(defconstant	*obsolete-entity-format*
	"~&Entity ~A ~@[(originally defined in ~a)~] is obsolete."
	"arguments: entity name, file name"
)
(defconstant	*overloaded-concrete-format*
	"~&The following ~As reify more than one ~A: ~A."
	"argument: concrete type name, abstract type name, concrete instances"
)
(defconstant	*uninitialized-entity-format*
	"~&Entity ~A is not initialized."
	"argument: entity name"
)


;; declarations
(declaim (string *bad-reporter-type-format* *mismatched-io-format*
		 *missing-file-format* *obsolete-entity-format*
		 *overloaded-concrete-format* *uninitialized-entity-format*))


;;; the :around method manages validation, the primary methods ensure that
;;; each slot value is valid, whereas the :after methods enforce inter-slot
;;; constraints.

(defmethod validate :around ((entity entity))
	"validate only if necessary; return the instance or signal an error"
	(with-accessors ((name name)
			 (status status)
			 (defined-by defined-by)) entity
		(ecase status
		  (:referenced	(draco-error
					*uninitialized-entity-format* name))
		  (:initialized	(setf status :validating)
				(call-next-method)
				(setf status :validated))
		  (:validated	)
		  (:validating	)
		  (:obsolete	(draco-error
					*obsolete-entity-format* name
					(file (car defined-by))))
		))
	entity)


(defmethod validate ((cons cons))
	(map nil #'validate (true-list cons))
	cons)

(defmethod validate ((constraint constraint))
	constraint)

(defmethod validate ((draco-class draco-class))
	draco-class)

(defmethod validate ((entity entity))
	"necessary condition: an instance is valid only if its slot values are"
	(dolist (slot-name (mapcar #'slot-definition-name
				   (class-slots (class-of entity))))
		(aver (moniker slot-name))
		(let ((slot-value (and (slot-boundp entity slot-name)
				       (slot-value entity slot-name))))
		     (aver (t slot-value))
		     (typecase slot-value
			(pathname	(with-open-stream
					  (stream (open slot-value
						    :if-does-not-exist nil))
					  (unless stream
						  (draco-error
							*missing-file-format*
							slot-value))))
			(t		(validate slot-value))
		     ))))

(defmethod validate ((function function))
	function)

(defmethod validate ((null null))
	t)

(defmethod validate ((number number))
	number)

(defmethod validate ((package package))
	package)

(defmethod validate ((string string))
	string)

(defmethod validate ((symbol symbol))
	symbol)

(defmethod validate ((vector vector))
	(map nil #'validate vector)
	vector)


;; internal macro definition
(macrolet
  ((define-constraint
	(&key (name		(gentemp 'constraint *Draco-package*))
	      (documentation	(draco-error *missing-keyword-format*
				  "define-constraint" :documentation))
	      (constraint	(draco-error *missing-keyword-format*
				  "define-constraint" :constraint)))
	"create and return an instance of constraint"
	`#!(make-instance 'constraint
		:name		',name
		:documentation	',documentation
		:constraint	(compile nil
					 (list 'lambda nil ',constraint)))))


  ;; here's johnny!
  (define-constraint
    :name		data-type-file-type-correspondence
    :documentation	"each file type may only reify one data type"
    :constraint
	(let* ((file-types (coerce (apply #'concatenate '(vector file-type)
					  (mapcar #'file-types
						  (instances 'data-type)))
				   'list))
	       (duplicates (nset-difference file-types
					    (remove-duplicates file-types))))
	      (aver (list file-types duplicates))
	      (when duplicates
		    (draco-error *overloaded-concrete-format*
				 "file type" "data type" duplicates)))
  )

  (define-constraint
    :name		primitive-implementation-correspondence
    :documentation	"each implementation may only reify one primitive"
    :constraint
	(let* ((impls (coerce (apply #'concatenate '(vector implementation)
				     (mapcar #'implementations
					     (instances 'primitive)))
			      'list))
	       (duplicates (nset-difference impls
					    (remove-duplicates impls))))
	      (aver (list impls duplicates))
	      (when duplicates
		    (draco-error *overloaded-concrete-format*
				 "implementation" "primitive" duplicates)))
  )

  (define-constraint
    :name		primitive-implementation-input-output-correspondence
    :documentation	"the data types should map 1-1 onto the file types"
    :constraint
	(dolist (primitive (instances 'primitive))
		(aver (primitive primitive))
		(dolist (io-fn '(input output))
			(aver (moniker io-fn))
			(map nil
			     #'(lambda (impl)
				 (aver (implementation impl))
				 (unless (set-equal
					   (coerce (funcall io-fn primitive)
						   'list)
					   (map 'list
						#'data-type
						(funcall io-fn impl)))
					 (draco-error *mismatched-io-format*
						      io-fn
						      (name primitive)
						      (name impl))))
			     (implementations primitive))))
  )

  (define-constraint
    :name		reporter-type-correspondence
    :documentation	"a file-type and its reporter must have the same type"
    :constraint
	(dolist (file-type (instances 'file-type))
		(aver (file-type file-type))
		(with-accessors ((name name)
				 (reporter reporter)
				 (reporter-type reporter-type)) file-type
		  (etypecase reporter
		    (null	    )
		    (function	    (when (eq reporter *default-reporter*)
					  (unless (eq reporter-type
						      *default-reporter-type*)
						  (draco-error
						    *bad-reporter-type-format*
						    name))))
		    (KV-reporter    (unless (eq reporter-type
						(reporter-type reporter))
					    (draco-error
						*bad-reporter-type-format*
						name)))
		  )))
  )
) ; macrolet
