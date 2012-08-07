;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	file-datum definitions
;;;

(in-package draco)


;; constants (duh!)
(defconstant	*KV-key-order-property*
	'KV-keyword-order-number
	"
	specifies the keyword's position in the user-specified
	keyword list; sort KV-pairs according to this order"
)

;; format string
(defconstant	*entry-line-format*
	"~%    ~8A: ~15,0T ~A"
	"arguments: keyword, value"
)


;; declarations
(declaim (type moniker *KV-key-order-property*))
(declaim (string *entry-line-format*))

(declaim (function find-file-datum (file-spec) (optional file-datum)))
(declaim (function get-file-data (pathname) list))
(declaim (function get-file-datum (string &rest args) file-datum))


;; internal method definitions
(defmethod get-KV-pairs ((file-datum file-datum) data-directory)
	"use KV-reporter to fill file-datum's KV-pairs slot"
	(aver (pathname data-directory))
	(with-accessors ((file-name file-name)
			 (file-type file-type)
			 (KV-pairs KV-pairs)) file-datum
	  (with-accessors ((reporter reporter)) file-type
	    (when (typep reporter 'KV-reporter)
		  (with-accessors ((lex-function lex-function)
				   (report-keywords report-keywords)) reporter
		    (set-desired-keywords report-keywords)
		    (let ((keyword nil)
			  (value nil)
			  (ls-stream nil)
			  (ignored-stream nil)
			  (pid nil))
			 (aver ((optional string) keyword value)
			       ((optional stream) ls-stream ignored-stream)
			       ((optional integer) pid))
			 (unwind-protect
			   (with-open-stream
				(stream
				 (etypecase lex-function
				   (function-spec
					(setq ls-stream
					      (funcall
						lex-function file-name)))
				   (file-spec
					(multiple-value-setq
						(ls-stream ignored-stream pid)
						(run-shell-command
						  (concatenate 'string
						    (namestring lex-function)
						    ;; sick hack to cover for
						    ;; Fortran's inability to
						    ;; follow symbolic links
						    " `"
						    (namestring
						      (make-pathname
							:defaults *tools-dir*
							:name	  "truename"))
						    " "
						    (namestring file-name)
						    "`")
						  :wait nil  :output :stream)))
				 )) ; etypecase/stream
				(aver (stream stream))
				(loop (setq keyword (read-line stream nil nil)
					    value   (read-line stream nil nil))
				      (when (null keyword)	; eof test
					    (setf KV-pairs
						  (delete-duplicates KV-pairs
							:key		#'car
							:from-end	t))
					    (return))
				      (unless (string= keyword "")
					;; here we assume that keywords
					;; are always uppercase.  bogus!!
					(setq keyword
					      (intern (string-right-trim
							" " keyword)))
					(when (get keyword
						   *KV-key-order-property*)
					      (push (cons keyword value)
						    KV-pairs)))))
			   (ignore-errors
				(when pid (blocking-wait-with-pid pid)))
			 )))))) ; unwind-protect/let/.../with-accessors
	(values))


(defmethod make-description ((file-datum file-datum))
	"fill file-datum's description slot"
	(with-accessors ((KV-pairs KV-pairs)
			 (description description)) file-datum
		(let ((entry-lines nil))
		     (aver (list entry-lines))
		     (dolist (KV-pair KV-pairs)
			     (aver (cons KV-pair))
			     (push (format nil *entry-line-format*
					   (car KV-pair) (cdr KV-pair))
				   entry-lines))
		     (setf description
			   (apply #'concatenate 'string
				  (nreverse entry-lines)))))
	(values))


(flet
  ((get-sort-key (KV-pair)
	"return sort key (integer) for (desired) keyword"
	(aver (cons KV-pair))
	(or (get (car KV-pair) *KV-key-order-property*)
	    most-positive-fixnum)))

  (declare (function get-sort-key (cons) integer))

  (defmethod sort-KV-pairs ((file-datum file-datum))
	"sort file-datum's KV-pairs slot"
	(with-accessors ((KV-pairs KV-pairs)) file-datum
		(setf KV-pairs
		      (stable-sort KV-pairs #'< :key #'get-sort-key)))
	(values))
) ; flet


;; local function definitions
(labels
  ((find-file-datum-1 (file-name candidate)
	"find-file-datum workhorse"
	(aver (string file-name)
	      ((optional file-datum) candidate))
	(let ((file-types (if candidate
			      (then (sub-types (file-type candidate)))
			      (else *root-file-types*)))
	      (recognized-file-type nil)
	      (description nil))
	     (aver (list file-types)
		   ((optional file-type) recognized-file-type)
		   ((optional string) description))
	     (dolist (file-type file-types)
		     (aver (file-type file-type))
		     (when (setq description
				 (invoke-recognizer file-type file-name))
			   (setq recognized-file-type
				 (get-file-datum file-name
				   :file-type	file-type
				   :description	description))
			   (return)))
	     (if recognized-file-type
		 (then (find-file-datum-1 file-name recognized-file-type))
		 (else candidate))))


   (invoke-recognizer (file-type file-name)
	"invoke a file type recognizer"
	(aver (file-type file-type)
	      (string file-name))
	(with-accessors ((recognizer recognizer)) file-type
		(etypecase recognizer
		  (function	(funcall recognizer file-name))
		  (file-spec	(car (second
					(multiple-value-list
					  (shell (concatenate 'string
						   (namestring recognizer)
						   " "
						   file-name)
						 :output	nil
						 :error		nil
						 :ignore-status	t)))))
		))))


  ;; local declarations
  (declare (function find-file-datum-1
		     (string (optional file-datum)) (optional file-datum)))
  (declare (function invoke-recognizer
		     (file-type string) (optional string)))


  ;; internal function definition
  (defun find-file-datum (file-spec)
	"find the file-type of file-spec"
	(aver (file-spec file-spec))
	(let ((file-name (namestring (pathname file-spec))))
	     (aver (string file-name))
	     (or (find file-name (instances 'file-datum)
			:key #'file-name  :test #'string=)
		 (find-file-datum-1 file-name nil)
		 (get-file-datum file-name))))
); labels


;; internal function definitions
(defun get-file-data (directory)
	"find file-type of directory files that are newer than the
	directory's inventory; return nil if there are no such files,
	non-nil otherwise."
	(aver (pathname directory))

	(let ((file-data-created-p nil)
	      (inventory-name (file-namestring (make-pathname
						 :name *inventory-name*
						 :type *inventory-type*)))
	      (ls-stream nil)
	      (ignored-stream nil)
	      (pid nil)
	      ;; the following let initializations are bogus
	      (file-datum nil)
	      (file-name nil)
	      (file-pathname *tools-dir*))
	     (aver (boolean file-data-created-p)
		   (string inventory-name)
		   ((optional stream) ls-stream ignored-stream)
		   ((optional integer) pid)
		   ((optional file-datum) file-datum)
		   ((optional string) file-name)
		   (pathname file-pathname))

	     (unwind-protect
		(with-open-stream
		  (stream (multiple-value-setq
			     (ls-stream ignored-stream pid)
			     (run-shell-command
				(concatenate 'string
					"ls -t " (namestring directory))
				:wait nil  :output :stream)))
		  (aver (stream stream))
		  (loop					; through ls output
			(setq file-name (read-line stream nil nil))

			;; exit loop if inventory/eos encountered
			(when (or (null file-name)
				  (string= file-name inventory-name))
			      (return))

			(setq file-data-created-p t)

			(setq file-pathname
			      (make-pathname
				:defaults	directory
				:name		(pathname-name file-name)
				:type		(pathname-type file-name)))

			(setq file-datum (find-file-datum file-pathname))

			(with-accessors ((file-type file-type)) file-datum
				(when file-type
				      (pushnew file-datum
					       (file-data file-type))))))
		(ignore-errors (when pid (blocking-wait-with-pid pid)))
	     ) ; unwind-protect

	     (dolist (file-type (instances'file-type))
		     (aver (file-type file-type))
		     (setf (file-data file-type)
			   (nreverse (file-data file-type))))

	     file-data-created-p))


(defun get-file-datum (file-name &rest args)
	"
	return the file-datum instance corresponding
	to file-name and args creating it if necessary"
	(aver (string file-name)
	      (list args))
	(let ((instance (find file-name (instances 'file-datum)
				:key #'file-name :test #'string=)))
	     (aver ((optional file-datum) instance))
	     (if instance
		 (then (apply #'reinitialize-instance instance args))
		 (else (apply #'make-instance 'file-datum
				:file-name	file-name
				args)))))
