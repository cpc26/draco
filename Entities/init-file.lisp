;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	initialization-file definitions
;;;

(in-package draco)

#!(export '(set-data-directory update-inventory))


;; constants (duh!)
;; format string
(defconstant	*load-circularity-format*
	"~&initialization-file circularity detected (~A)"
	"argument: initialization-file"
)
(defconstant	*load-condition-format*
	"~&Warning: load error (~A)~
	 ~%Warning: Draco may be in an inconsistent state."
	"argument: condition"
)


;; declarations
(declaim (string *load-circularity-format* *load-condition-format*))

(declaim (function set-data-directory (&optional file-spec) string))


;; internal class-specific method definitions
(let ((defined-by nil))
     (aver (list defined-by))

     (defmethod fire-load :around
		((initialization-file initialization-file) &key (force nil))
	"
	if initialization-file is up-to-date and :force is nil (default),
	do nothing and return nil; otherwise (re)load it setting the
	defined-by slot value appropriately.  warn the user if load errors
	are encountered."
	(aver (boolean force))
	(let ((old-defined-by defined-by))
	     (aver (list old-defined-by))
	     (when (find initialization-file defined-by)
		   (draco-error *load-circularity-format* initialization-file))
	     (unless (defined-by initialization-file)
		     (setf (defined-by initialization-file) defined-by))
	     (unwind-protect
		(progn
		  (push initialization-file defined-by)
		  (multiple-value-bind (status condition)
				       (call-next-method)
		    (aver (boolean status)
			  ((optional condition) condition))
		    (when condition
			  (format *error-output*
				  *load-condition-format* condition))
		    (dolist (entity (instances 'imported-entity))
			    (aver (imported-entity entity))
			    (unless (defined-by entity)
				    (when condition
					  ;; if an old entity references
					  ;; a new entity defined before
					  ;; the load failed, validation
					  ;; will still fail.
					  (let ((class (class-of entity)))
					       (aver (standard-class class))
					       (setf (instances class)
						     (delete
							entity
							(instances class)))))
				    (when (and status (initialized entity))
					  (setf (defined-by entity)
						defined-by))))
		    (values status condition)))
		 (setq defined-by old-defined-by))))
) ; let


(defmethod shared-initialize :after
	   ((initialization-file initialization-file) slot-names &rest initargs)
	"create clear-fn and handle immediate loads"
	(declare (ignore initargs slot-names))
	(setf (clear-fn initialization-file)
	      (compile (gentemp 'fire-clear-fn 'Draco)
		`(lambda ()
			(dolist (instance (instances ,initialization-file))
				(aver (imported-entity instance))
				(when (initialized instance)
				      (setf (status instance) :obsolete)))
			(values))))
	(when (load-immediately initialization-file)
	      (fire-load initialization-file :force t)))


(let ((data-directory nil)
      (data-directory-init-file nil)
      (data-inventory nil))
     (aver ((optional string) data-directory)
	   ((optional initialization-file)
		data-directory-init-file data-inventory))

     ;; local function definitions
     (labels
	((initialize-data ()
		"
	complete initialization of classes specified by
	class-names, validate, and enforce all constraints"
		(let ((classes (list (find-class 'entity))))
		     (aver (cons classes))
		     (setq classes
			   (delete-duplicates
				(nconc classes
				       (mapcan #'class-subclasses classes))))
		     (map nil #'initialize-instances classes)
		     (when (find 'file-type classes :key #'class-name)
			   (setf *root-file-types*
				 (mapcan
					#'(lambda (file-type)
					    (aver (file-type file-type))
					    (and (null (super-type file-type))
						 (list file-type)))
					(instances 'file-type))))
		     ;; the next three forms may be evaluated in parallel,
		     ;; because only the status slot should be modified
		     ;; after this point.
		     (map nil #'funcall (mapcar #'constraint
						(instances 'constraint)))
		     (map nil #'validate classes)
		     (map nil #'validate
			  (mapcan #'copy-seq (mapcar #'instances classes))))
		(values))

	 (load-all-data ()
		"
	if the data files are up-to-date, do nothing and return nil;
	otherwise (re)load the files, and return the most recent
	file-write-date."
		(let* ((init-files (instances 'initialization-file))
		       (data-init-file (find '*data-init-file* init-files
					     :key #'name))
		       (user-init-file (find '*user-init-file* init-files
					     :key #'name)))
		      (aver (list init-files)
			    ((optional initialization-file)
				data-init-file user-init-file))
		      ;; load the data directory's init. file (if necessary)
		      (and data-init-file
			   (fire-load data-init-file)
			   user-init-file
			   (fire-load user-init-file :force t))
		      ;; load the remaining initialization-files (if necessary)
		      (map nil #'fire-load
			(remove data-init-file
				(instances 'initialization-file)))
		      ;; initialize and validate data
		      (initialize-data))))


	;; local function declarations
	(declare (function initialize-data (&rest list))
		 (function load-all-data ()))


	;; exported function definition
	(defun set-data-directory (&optional (directory-spec (cwd)))
	  "
	the specified directory becomes the user's current directory
	and it is returned as a string.  if no directory is specified,
	the current directory is returned."
	  (aver (file-spec directory-spec))
	  (unless (equalp data-directory directory-spec)
	     (let ((*package* (find-package 'draco)))
		  (declare (special *package*))
		  (cwd directory-spec)
		  (setq data-directory directory-spec)
		  ;; forget previous data directory's inventory
		  (setf (instances 'file-datum) nil)
		  (setf (instances 'initialization-file)
			(delete data-inventory
				(instances 'initialization-file)))
		  ;; undo previous data directory's initialization-file
		  (when data-directory-init-file
			(fire-clear data-directory-init-file)
			(setf (instances 'initialization-file)
			      (delete data-directory-init-file
				      (instances 'initialization-file)))
			(setq data-directory-init-file nil))
		  ;; define the new data directory initialization-file
		  ;; if it exists.
		  (with-open-file (stream
				   (make-pathname
					:defaults	directory-spec
					:name		*init-file-name*
					:type		*init-file-type*)
				   :if-does-not-exist nil)
			(aver ((optional stream) stream))
			(when stream
			      (setq data-directory-init-file
				    (define-internal-initialization-file
					:name	*data-init-file*
					:documentation
					 	"data directory init. file"
					:file	(truename stream)))))
		  ;; load everything but the data inventory
		  (load-all-data)
		  ;; load the data inventory, creating one if necessary
		  (setq data-inventory
			(define-internal-initialization-file
				:name		*data-directory-inventory*
				:documentation	"data directory inventory file"
				:file		(make-pathname
						  :name	*inventory-name*
						  :type	*inventory-type*)
				:update-fn	#'make-inventory
				:update-parameters
					'(:documentation "Generated by Draco"
					  :force t)
			))
		  (update-inventory)))
	  (setq data-directory (cwd)))
     ) ; labels


     (defun update-inventory ()
	"update the data directory inventory"
	(fire-update data-inventory))
) ; let


;; define user-dependent initialization file
(eval-when (:load-toplevel)
	(with-open-file (stream
			 (make-pathname
				:defaults	(concatenate 'string
							(getenv "HOME") "/")
				:name		*init-file-name*
				:type		*init-file-type*)
			 :if-does-not-exist nil)
		(aver ((optional stream) stream))
		(when stream
		      (define-internal-initialization-file
			:name		*user-init-file*
			:documentation	"user's home directory init. file"
			:file		(truename stream))))
)
