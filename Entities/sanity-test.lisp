;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	Draco sanity test
;;;
;;;	look ma, no data!  this test summarizes the contents of
;;;	arbitrary directories.  the script that prints this summary
;;;	is then deleted (and words of wisdom are printed).  a number
;;;	of useful entities are defined here.
;;;

(in-package draco)


;; entity definitions
(define-internal-data-type
	:name		data-set
	:documentation	"science data set"
	:file-types	(directory)
)

(define-internal-data-type
	:name		script
	:documentation	"executable script"
	:file-types	(shell-script)
)

(define-internal-file-type
	:name		shell-script
	:documentation	"executable shell script"
	:super-type	Unix
	:recognizer	(make-pathname	:defaults  *tools-dir*
					:name	   "script-p"
					:type	   "sh")
)

(define-internal-implementation
	:name			clean-up
	:documentation		"delete previously generated shell scripts"
	:input			shell-script
)

(define-internal-implementation
	:name			summarize-data
	:documentation		"print a summary of a directory's contents"
	:input			directory
	:output			shell-script
)

(define-internal-primitive
	:name			clean-up
	:documentation		"print a summary of a data-set's contents"
	:reconciled-input	((script :disjunctive))
	:implementations 	(clean-up)
)

(define-internal-primitive
	:name			summarize-data
	:documentation		"print a summary of a data-set's contents"
	:reconciled-input	((data-set :disjunctive))
	:output			script
	:implementations 	(summarize-data)
)

(define-internal-procedure
	:name		sanity-test
	:documentation	"Draco data-free sanity test"
	:primitives	(summarize-data clean-up)
)


;; format string
(defconstant	*test-clean-up-comment-1*
	"echo Draco is designed to incrementally update its data inventories,"
	"no arguments"
)
(defconstant	*test-clean-up-comment-2*
	"echo but this feature is not implemented yet. \"\" File deletions such"
	"no arguments"
)
(defconstant	*test-clean-up-comment-3*
	"echo as the ones performed by this script are ill-advised because"
	"no arguments"
)
(defconstant	*test-clean-up-comment-4*
	"echo they leave the \"directory's\" inventory in a bad state. \"\" To insure"
	"no arguments"
)
(defconstant	*test-clean-up-comment-5*
	"echo proper operation, periodically delete your data inventories."
	"no arguments"
)
(defconstant	*test-clean-up-format*
	"rm ~A"
	"string argument: file name"
)
(defconstant	*test-clean-up-log-comment*
	"you weren't using this were you?"
	"no arguments"
)
(defconstant	*test-clean-up-log-format*
	"rm $LOG_FILE"
	"no arguments"
)
(defconstant	*test-clean-up-newline*
	"echo"
	"no arguments"
)
(defconstant	*test-data-summary-format*
	"echo ~D file~:P of type ~A."
	"arguments: integer, file-type name (symbol)"
)


;; declaration
(declaim (string *test-clean-up-comment-1* *test-clean-up-comment-2* 
		 *test-clean-up-comment-3* *test-clean-up-comment-4* 
		 *test-clean-up-comment-5* *test-clean-up-format*
		 *test-clean-up-log-comment* *test-clean-up-log-format*
		 *test-clean-up-newline* *test-data-summary-format*))


;; implementation method definitions
(defmethod clean-up ((stream stream) shell-script)
	"delete shell-script"
	(aver (file-spec shell-script))

	(add-command stream *test-clean-up-newline*)
	(add-command stream *test-clean-up-comment-1*)
	(add-command stream *test-clean-up-comment-2*)
	(add-command stream *test-clean-up-comment-3*)
	(add-command stream *test-clean-up-comment-4*)
	(add-command stream *test-clean-up-comment-5*)
	(add-command stream *test-clean-up-newline*)

	(add-comment stream *test-clean-up-log-comment*)
	(add-command stream *test-clean-up-log-format*)

	(add-command stream
		(format nil *test-clean-up-format* (namestring shell-script)))
	(add-command stream
		(format nil *test-clean-up-format*
			(namestring (make-pathname
					:defaults	shell-script
					:name		*inventory-name*
					:type		*inventory-type*))))
	(add-command stream
		(format nil *test-clean-up-format* (namestring stream)))
	nil)


(defmethod summarize-data ((stream stream) directory)
	"summarize directory's contents"
	(aver (file-spec directory))
	(dolist (file-type (instances 'file-type))
		(aver (file-type file-type))
		(with-accessors ((name name)
				 (file-data file-data)) file-type
			(add-command stream
				(format nil *test-data-summary-format*
					(length file-data) name))))
	(add-comment stream *test-clean-up-log-comment*)
	(add-command stream *test-clean-up-log-format*)
	(list (namestring stream)))
