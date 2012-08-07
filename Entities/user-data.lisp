;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	user-data functions
;;;

(in-package draco)


;; constant (duh!)
;; format string
(defconstant	*bad-io-tilde-format*
	"~&no default value for ~S; possible file types are: ~A"
	"arguments: \"~in\" or \"~out\", file-type list"
)


;; declarations
(declaim (string *bad-io-tilde-format*))

(declaim (function translate-cmd-string
		   (string
		    &optional signature signature (optional implementation))
		   command))
(declaim (function translate-cmd-strings
		   ((or sequence string)
		    &optional signature signature (optional implementation))
		   commands))


;; class-specific initialization method (internal)
(defmethod initialize-entity ((user-data user-data))
	"initialize user-data reconcile slot"
	(with-accessors ((file-type file-type)
			 (implementation implementation)
			 (reconcile reconcile)) user-data
		(when implementation
		      (setf reconcile
			    (cdr (assoc (data-type file-type)
					(reconciled-input
					  (primitive implementation)))))))
	(values))


(labels
  ;; local function definitions
  ((is-tilde-delimiter (character)
	"
	returns T if character is a tilde-directive delimiter
	(#\\\", #\\), or whitespace); returns nil otherwise."
	(aver (character character))
	(or (char= character #\")
	    (char= character #\))
	    (iswhite character)))

   (make-command-elements (cmd-string input output implementation)
	"returns the list of command elements corresponding to cmd-string."
	(aver (string cmd-string)
	      (signature input output)
	      ((optional implementation) implementation))
	(let ((tilde-start (position #\~ cmd-string)))
	     (aver ((optional fixnum) tilde-start))
	     (if tilde-start
		 (then (let ((tilde-end (or (position-if
						#'is-tilde-delimiter cmd-string
						:start tilde-start)
					    (length cmd-string))))
			    (aver (fixnum tilde-end))
			    (nconc
				(list (subseq cmd-string 0 tilde-start)
				      (parse-tilde
					(subseq
					  cmd-string tilde-start tilde-end)
					input output implementation))
				(make-command-elements
					(subseq cmd-string tilde-end)
					input output implementation))))
		 (else (list cmd-string)))))

   (parse-io-tilde (tilde input lb-position file-types implementation)
	"
	parses a Draco tilde directive that begins with \"~in\" or \"~out\".
	if the tilde expression is not recognized, e.g. if there is no
	terminating bracket, it is returned.  otherwise, a user-data
	instance is returned."
	(aver (string tilde)
	      (boolean input)
	      ((optional fixnum) lb-position)
	      (signature file-types)
	      ((optional implementation) implementation))
	(let* ((rb-position (and lb-position
				 (position #\] tilde :start lb-position)))
	       (file-type-name (and rb-position
				    (intern (string-upcase
						(subseq tilde
							(1+ lb-position)
							rb-position))))))
	      (aver ((optional fixnum) rb-position)
		    (symbol file-type-name))
	      (unless file-type-name
		      (unless (= 1 (length file-types))
			      (draco-error *bad-io-tilde-format*
					   tilde (coerce file-types 'list)))
		      (setq file-type-name
			    (name (svref file-types 0))))
	      (make-instance 'user-data
		:name		file-type-name
		:file-type	(get-instance 'file-type :name file-type-name)
		:input		input
		:implementation	(and input implementation))))

   (parse-tilde (tilde input output implementation)
	"
	parses a Draco tilde directive.  if the tilde expression cannot
	be parsed, it is returned.  if it is \"~log\", :log is returned.
	otherwise, a user-data instance is returned."
	(aver (string tilde)
	      (signature input output)
	      ((optional implementation) implementation))
	(let* ((lb-position (position #\[ tilde)))
	      (aver ((optional fixnum) lb-position))
	      (case (intern (string-upcase (subseq tilde 1 lb-position)))
		(in	(parse-io-tilde
				tilde t lb-position input implementation))
		(out	(parse-io-tilde tilde nil lb-position output nil))
		(log	:log)
		(t	tilde)))))


  ;; local function declarations
  (declare (function is-tilde-delimiter (character) boolean)
	   (function make-command-elements
		     (string signature signature (optional implementation))
		     list)
	   (function parse-io-tilde
		     (string boolean (optional fixnum) signature
		      (optional implementation)) command-element)
	   (function parse-tilde
		     (string signature signature (optional implementation))
		     command-element))


  ;; internal function definitions
  (defun translate-cmd-string
	 (cmd-string &optional (input #()) (output #()) (implementation nil))
	"
	translates the Draco tilde directives in a command
	string and returns the corresponding command."
	(aver (string cmd-string)
	      (signature input output)
	      ((optional implementation) implementation))
	(typed-vector 'command-element
		      (make-command-elements
			cmd-string input output implementation)))
) ; labels

					
(defun translate-cmd-strings
       (cmd-strings &optional (input #()) (output #()) (implementation nil))
	"
	translates the Draco tilde directives in command
	strings and returns the corresponding commands."
	(aver ((or sequence string) cmd-strings)
	      (signature input output)
	      ((optional implementation) implementation))
	(when (stringp cmd-strings)
	      (setq cmd-strings (list cmd-strings)))
	(map 'commands
	     #'(lambda (cmd-string)
		       (aver (string cmd-string))
		       (translate-cmd-string
				cmd-string input output implementation))
	     cmd-strings))
