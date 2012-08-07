;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	Draco inventory writer
;;;

(in-package draco)

#!(export '(make-inventory))


;; constants (duh!)
;; format strings
(defconstant	*file-type-header-format*
	"~%~%;;; ~A files --~%~%"
	"argument: file type name"
)
(defconstant	*in-package-format*
	"~&~%(in-package draco)~%"
)
(defconstant	*inventory-header-format*
	"~&;;; Inventory for ~A~%~
	;;; Generated on ~A~%~
	;;; Draco version ~A~%~
	~@[~&~%;;; ~A~%~]"
	"arguments: directory name, date string, version string, documentation"
)


;; declarations
(declaim (string *file-type-header-format* *in-package-format*
		 *inventory-header-format*))

(declaim (function make-inventory
		   (stream-spec &key documentation entries-only force)))


;; local function definitions
(labels
  ((begin-inventory (inventory directory documentation
		    &optional (entries-only nil))
	"write an inventory header"
	(aver (stream inventory)
	      (pathname directory)
	      ((optional string) documentation)
	      (boolean entries-only))
	(unless entries-only
		(format inventory *inventory-header-format*
			(string-right-trim "/" (format nil "~A" directory))
			(date) *version* documentation))
	(format inventory *in-package-format*)
	(values))


   (write-file-type-report (file-type directory entries-only
			    &optional (report-stream *standard-output*))
	"invoke a file type-specific reporter"
	(aver (file-type file-type)
	      (pathname directory)
	      (boolean entries-only)
	      (stream report-stream))
	(unless entries-only
		(write-file-type-report-header file-type report-stream))
	(let* ((reporter (reporter file-type))
	       (reporter-fn (etypecase reporter
				(function	reporter)
				(KV-reporter	(reporter reporter))
			    )))
	      (aver (function reporter-fn))
	      (ecase (reporter-type file-type)
		 (:c-sorted	(funcall reporter-fn
					 (file-data file-type)
					 directory report-stream))
		 (:f-sorted	(dolist (file-datum (file-data file-type))
					(aver (file-datum file-datum))
					(funcall reporter-fn
						 file-datum directory
						 report-stream)))
	      ))
	(values))


   (write-file-type-report-header
	(file-type &optional (report-stream *standard-output*))
	"write a file type-specific reporter header"
	(aver (file-type file-type)
	      (stream report-stream))
	(format report-stream *file-type-header-format* (name file-type))
	(values)))


  ;; local declarations
  (declare (function begin-inventory
		     (stream pathname (optional string) &optional boolean))
	   (function write-file-type-report
		     (file-type pathname boolean &optional stream))
	   (function write-file-type-report-header
		     (file-type &optional stream)))


  ;; exported function definition
  (defun make-inventory (inventory
			 &key (documentation nil)
			      (entries-only nil)
			      (force nil))
	"
	insure data directory inventory is up-to-date; if force
	is non-nil, blindly generate a new inventory file."
	(aver (stream-spec inventory)
	      ((optional string) documentation)
	      (boolean entries-only force))

	(let ((directory (set-data-directory))
	      (unrecognized nil))
	     (aver (file-spec directory)
		   (list unrecognized))

	     (when (or (get-file-data directory) force)
		   (with-open-stream
		     (stream (etypecase inventory
				(stream		inventory)
				((eql t)	(make-synonym-stream
							*standard-output*))
				(file-spec	(open inventory
						  :direction :output
						  :if-exists :new-version))
			     ))
		     (aver (stream stream))

		     (begin-inventory
			stream directory documentation entries-only)

		     (dolist (file-type (instances 'file-type))
			     (aver (file-type file-type))
			     (when (and (file-data file-type)
					(reporter file-type))
				   (write-file-type-report
				     file-type directory entries-only stream)))

		     (when (setq unrecognized
				 (find-every #'null (instances 'file-datum)
					     :key #'file-type))
			   (format stream *file-type-header-format*
				   ";;; UNRECOGNIZED files --")
			   (dolist (file-datum unrecognized)
				   (aver (file-datum file-datum))
				   (write-inventory-mystery-entry
					(file-name file-datum) stream))))))
	(values))
) ; labels
