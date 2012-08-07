;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	Draco generic keyword-value (KV) file reporter
;;;
;;;	this reporter takes the output of a lexical analyzer and creates
;;;	report entries from the desired KV pairs.
;;;
;;;	the format for a KV file report is:
;;;
;;;	<file name>:
;;;		<keyword 1> =	<keyword 1 value>
;;;		<keyword 2> =	<keyword 2 value>
;;;			. . .
;;;		<keyword N> =	<keyword N value>
;;;
;;;	if a desired keyword appears more than once in a KV file, all of
;;;	its values are given in separate lines.  all lines are listed in
;;;	the order which they occur in the file.
;;;
;;;	there is the usual pain-in-the-ass problem of getting the spacing
;;;	to look good or at least reasonable.  this should be documented
;;;	but it's not, sorry.
;;;
;;;	the KV file reporter is designed to work with two kinds of
;;;	lexical analyzers: shell commands and CL functions.  a lexical
;;;	analyzer must create a stream of KV pairs:
;;;
;;;	<keyword><newline><value><newline>
;;;
;;;	the lexical analyzer need not strip comments or syntactic sugar,
;;;	e.g. equal signs, from the values, but the report will probably
;;;	look better if it does.
;;;
;;;	the macro define-report-keywords is provided for use in user
;;;	initialization files.
;;;

(in-package draco)

#!(export '(define-report-keywords))


;; constants (duh!)
;; format strings
(defconstant	*creation-format*
	"~&Compiled Keyword-Value Reporter ~A (~A)"
	"argument: KV-reporter name, KV-reporter type"
)
(defconstant	*entry-leader-format*
	"~&~A =~%"
	"argument: file name"
)
(defconstant	*keyword-list-doc-format*
	"symbol bound to ~A's desired keyword list"
	"argument: KV-reporter name"
)


;; declarations
(declaim (string *creation-format* *entry-leader-format*
		 *keyword-list-doc-format*))

(declaim (function set-desired-keywords (list)))


;; internal class-specific method definitions
(defmethod make-KV-reporter-fn
	   ((KV-reporter KV-reporter) (reporter-type (eql :c-sorted)))
	"create and return the specified :c-sorted KV-reporter function"
	(with-accessors ((name name)
			 (lex-function lex-function)) KV-reporter
		(eval
		  `(declaim (function ,name (list pathname &optional stream))))
		(let* ((fkv-reporter-name (gentemp name *Draco-package*))
		       (fkv-reporter
			 (eval
			   `(define-KV-reporter
				:name		,fkv-reporter-name
				:documentation	"created by define-KV-reporter"
				:lex-function	,lex-function
				:reporter-type	:f-sorted))))
		      (aver (moniker fkv-reporter-name)
			    (KV-reporter fkv-reporter))
		      (compile name
			`(lambda (file-data data-directory
				  &optional (report-stream *standard-output*))
			   (aver (list file-data)
				 (pathname data-directory)
				 (stream report-stream))
			   (setf (report-keywords ,fkv-reporter)
				 (report-keywords ,KV-reporter))
			   (dolist (file-datum file-data)
				   (aver (file-datum file-datum))
				   (get-KV-pairs file-datum data-directory))
			   (map nil #'sort-KV-pairs file-data)
			   (map nil #'make-description file-data)
			   (let ((previous nil))
				(aver ((optional string) previous))
				(dolist (fd (stable-sort
						(copy-seq file-data)
						#'string<
						:key #'description))
					(aver (file-datum fd))
					(with-accessors
					  ((descr description)) fd
					  (when previous
						(unless (string=
								previous descr)
							(terpri report-stream)))
					  (print-readable-object
						fd report-stream)
					  (setq previous descr))))
			   (terpri report-stream)
			   (values))))
		(fdefinition name)))


(defmethod make-KV-reporter-fn
	   ((KV-reporter KV-reporter) (reporter-type (eql :f-sorted)))
	"create and return the specified :f-sorted KV-reporter function"
	(with-accessors ((name name)) KV-reporter
	  (eval `(declaim (function ,name (file-datum pathname
					   &optional stream))))
	  (compile name
	    `(lambda (file-datum data-directory
		      &optional (report-stream *standard-output*))
		(aver (file-datum file-datum)
		      (pathname data-directory)
		      (stream report-stream))
		(get-KV-pairs file-datum data-directory)
		(make-description file-datum)
		(print-readable-object file-datum report-stream)
		(values)))
	  (fdefinition name)))


(defmethod make-KV-reporter-fn :after
	   ((KV-reporter KV-reporter) (reporter-type symbol))
	"reporter-type-independent definitions"
	(with-accessors ((name name)
			 (doc-string doc-string)) KV-reporter
		(setf (documentation name 'function)
		      (or doc-string "generated via define-KV-reporter"))
		(when *load-verbose*
		      (format *diagnostic-stream* *creation-format*
			      name reporter-type)))
	(values))


(defmethod shared-initialize :after
	   ((KV-reporter KV-reporter) slot-names &rest initargs)
	"(re)initialize the :reporter slot"
	(declare (ignore slot-names))
	(aver (list initargs))
	(when (> (length initargs) 2)	; :name buys us 2 args
	      (with-accessors ((reporter-type reporter-type)
			       (reporter reporter)) KV-reporter
		(setf reporter
		      (and reporter-type
			   (make-KV-reporter-fn KV-reporter reporter-type))))))


;; internal function definition
(let ((*desired-keywords* nil))
     (aver (list *desired-keywords*))

     (defun set-desired-keywords (desired-keywords)
	"set KV file reporter desired keyword list"
	(aver (list desired-keywords))

	(dolist (keyword *desired-keywords*)
		(aver (moniker keyword))
		(remprop keyword *KV-key-order-property*))

	(setq *desired-keywords* nil)
 
	(let ((keyword-pos -1))
	     (aver (integer keyword-pos))
	     (dolist (keyword desired-keywords)
		     (aver (moniker keyword))
		     (setf (get keyword *KV-key-order-property*)
			   (incf keyword-pos))
		     (push keyword *desired-keywords*)))
	(values))
) ; let


;; exported macro definition
(defmacro define-report-keywords
	  (&key (reporter	(draco-error *missing-keyword-format*
				  "define-report-keywords" :reporter))
		(keywords	(draco-error *missing-keyword-format*
				  "define-report-keywords" :keywords)))
	"set the specified reporter's desired keyword list"

	`#!(setf (report-keywords (get-instance 'KV-reporter :name ',reporter))
		 (if (listp ',keywords)
		     (then ',keywords)
		     (else (list ',keywords)))))


	(dolist (file-type (instances 'file-type))
		(aver (file-type file-type))
		(setf (file-data file-type) nil))
	(setf (instances 'file-datum) nil)
