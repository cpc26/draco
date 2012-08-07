;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	(imported) entity instance definitions
;;;

(in-package draco)


;; declarations
(declaim (function get-instance
		   (moniker &rest cons &key name &allow-other-keys)
		   imported-entity))
(declaim (function get-instance-vector
		   (moniker (or sequence symbol)) (vector imported-entity)))


;; internal method definitions
(defmethod changed ((imported-entity imported-entity))
	"returns t only if imported-entity's status is :changed"
	(eql :changed (status imported-entity)))


(defmethod initialize-instances ((class draco-class))
	"finish initializing the instances of class and its subclasses"
	(aver (draco-class class))
	(map nil #'initialize-entity (instances class))
	(map nil #'initialize-instances (class-subclasses class))
	(values))


(defmethod initialized ((imported-entity imported-entity))
	"returns t only if imported-entity's status is :initialized"
	(eql :initialized (status imported-entity)))


(defmethod instances ((initialization-file initialization-file))
	"return list of instances defined-by initialization-file"
	(let ((instances nil))
	     (aver (list instances))
	     (dolist (instance (instances 'imported-entity))
		     (aver (imported-entity instance))
		     (when (find initialization-file (defined-by instance))
			   (push instance instances)))
	     instances))

(defmethod instances ((class-name symbol))
	"return list of instances of class named by class-name"
	(case class-name
	  (procedure	(find-every #'procedure
				    (instances (find-class 'primitive))))
	  (t		(let ((class (find-class class-name)))
			     (aver (draco-class class))
			     (mapcan #'copy-seq
				     (mapcar #'instances
					     (cons class (class-subclasses
								class))))))
	))

(defmethod (setf instances) (new-instances (class-name symbol))
	"set specified class instance list"
	(case class-name
	  (procedure	(setf (instances (find-class 'primitive))
			      (nconc (delete-if #'procedure
						(instances 'primitive))
				     new-instances)))
	  (t		(setf (instances (find-class class-name))
			      new-instances))
	))


;; internal function definitions
(defun get-instance (class-name &rest args
		     &key (name	(draco-error *missing-keyword-format*
					"get-instance" :name))
		     &allow-other-keys)
	"
	return the imported-entity instance corresponding
	to class-name and args creating it if necessary"
	(aver (moniker class-name)
	      (cons args)
	      (moniker name))

	(let ((instance (find name (instances class-name) :key #'name)))
	     (aver ((optional entity) instance))
	     (if instance
		 (then (apply #'reinitialize-instance instance args))
		 (else (apply #'make-instance class-name args)))))


(defun get-instance-vector (class-name instance-names)
	"
	return vector of imported-entity instances corresponding
	to instance-names.  instances are created if necessary."
	(aver (moniker class-name)
	      ((or list symbol) instance-names))

	(unless (typep instance-names 'sequence)
		(setq instance-names (vector instance-names)))

	(map (list 'vector (find-class class-name))
	     #'(lambda (instance-name)
		       (aver (moniker instance-name))
		       (get-instance class-name :name instance-name))
	     instance-names))
