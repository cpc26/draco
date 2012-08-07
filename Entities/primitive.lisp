;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	primitive definitions
;;;

(in-package draco)

#!(export '(add-command add-comment add-package-command script-tempnam
	    start-function))


;; constants (duh!)
;; format strings
(defconstant	*add-command-format*
	"~&~A"
	"argument: command string"
)
(defconstant	*add-comment-format*
	"~&~%# ~A"
	"argument: comment string"
)
(defconstant	*blank-line-format*
	"~&~%"
	"no arguments"
)
(defconstant	*cannot-distribute-format*
	"~&Draco:make-script: failed to distribute data for implementation ~A."
	"argument: implementation name"
)
(defconstant	*created-files-format*
	"~@[~&~%echo created files:~{~%echo \"    \" ~A~}~%~]"
	"argument: created file list"
)
(defconstant	*generating-script-format*
	"generating script ~a for ~a . . ."
	"arguments: script name, implementation name"
)
(defconstant	*invoking-script-format*
	"invoking script ~a for ~a . . ."
	"arguments: script name, implementation name"
)
(defconstant	*no-implementations-format*
	"~&~a:do: cannot find implementations for specified input."
	"argument: primitive name"
)
(defconstant	*script-header-format*
	"~&#!/bin/sh~%~
	# script title:~T~A~%~
	~@[# ~A~%~]~
	~@[# ~A~%~]~
	# creation date:~T~A~%~
	# Draco version:~T~A"
	"arguments: script title, summary, documentation, date, version"
)
(defconstant	*SIV-documentation*
	"Draco-generated; please do not edit!"
	"documentation slot value for SIV instances"
)
(defconstant	*too-little-data-format*
	"~&Draco:make-script: not enough data for ~A."
	"argument: implementation name or command"
)
(defconstant	*too-much-input-format*
	"~&Draco:make-script: too much input for ~A."
	"arguments: implementation name or command"
)


;; declarations
(declaim (string *add-command-format* *add-comment-format* *blank-line-format*
		 *cannot-distribute-format* *created-files-format*
		 *generating-script-format* *invoking-script-format*
		 *no-implementations-format* *script-header-format*
		 *SIV-documentation* *too-little-data-format*
		 *too-much-input-format*))

(declaim (function add-command (stream string)))
(declaim (function add-comment (string)))
(declaim (function add-package-command (stream string)))
(declaim (function find-SIVs (list signature) list))
(declaim (function make-SIV (impl-vector signature) SIV))
(declaim (function make-reconciled-input-alist ((or list symbol)) list))
(declaim (function merge-SIVs (&rest list) SIV))
(declaim (function script-tempnam (&optional string) string))


;; internal class-specific method definitions
(defmethod find-primitive-SIVs ((primitive primitive) signature)
	"
	returns a list of SIV structures corresponding to primitive.
	does not know about file-type conversion!"
	(aver (signature signature))
	(cdr (with-accessors ((primitives primitives)
			      (impl-map impl-map)) primitive
		(or (assoc signature impl-map :test #'match-signatures)
		    (let ((SIVs (find-SIVs (coerce primitives 'list)
					   signature)))
			 (aver (list SIVs))
			 (car (push (cons signature SIVs) impl-map)))))))


(let ((valid-impl-maps-p nil))
     (aver (boolean valid-impl-maps-p))

     (defmethod initialize-entity ((primitive primitive))
	"initialize primitive impl-map slot"
	(unless valid-impl-maps-p
		(with-accessors ((procedure procedure)
				 (implementations implementations)
				 (impl-map impl-map)) primitive
		  (setf impl-map
			(and (not procedure)
			     (map 'list
				  #'(lambda (impl)
				      (aver (implementation impl))
				      (with-accessors ((input input)
						       (output output)) impl
					(list input
					      (make-SIV (vector impl)
							(merge-signatures
							  input output)))))
				  implementations)))))
	(values))

     (defmethod initialize-instances :after
		((class (eql (find-class 'primitive))))
	"prevent redundant impl-map initialization"
	(setq valid-impl-maps-p t)
	(values))

     (defmethod shared-initialize :before
		((imported-entity imported-entity) slot-names &rest initargs)
	"trigger initialization of primitive impl-map slots"
	(declare (ignore slot-names))
	(aver (list initargs))
	(when (> (length initargs) 2)	; :name buys us 2 args
	      (setq valid-impl-maps-p nil)))
) ; let


(defmethod shared-initialize :after
	   ((primitive primitive) slot-names &rest initargs)
	"
	(re)initialize the associated implementation's primitive slots;
	create procedural primitive package and #'do"
	(declare (ignore slot-names))
	(aver (list initargs))
	(when (find :implementations initargs)
	      (map nil
		   #'(lambda (implementation)
			     (aver (implementation implementation))
			     (setf (primitive implementation) primitive))
		   (implementations primitive)))
	(when (procedure primitive)
	      (make-start-function primitive)))


(let* ((ad-hoc-file-data nil)
       (ad-hoc-signature #())
       (root-name ""))		; initial value not used
      (aver (list ad-hoc-file-data)
	    (signature ad-hoc-signature)
	    (string root-name))

      ;; local function definitions
      (labels
	((add-create-log-file (stream log-file)
		"add command creating log file to (end of) script"
		(aver (stream stream)
		      (file-spec log-file))
		(add-comment stream "create and initialize log file")
		(add-command stream
			(concatenate 'string
				"LOG_FILE=\"" (namestring log-file) "\""))
		(add-command stream "echo \"\" > $LOG_FILE")
		(values))


	 (add-created-files-message (stream script-name new-files)
		"add command telling user of created files to (end of) script"
		(aver (stream stream)
		      (string script-name)
		      (sequence new-files))
		(add-command stream
			(format nil *created-files-format*
			  (nconc (list script-name "$LOG_FILE") new-files)))
		(values))


	 (add-implementation (stream implementation)
		"add implementation commands to (end of) script"
		(aver (stream stream)
		      (implementation implementation))

		(with-accessors ((name name)
				 (draco-package draco-package)
				 (input input)
				 (_initialize-once _initialize-once)
				) implementation

			(when draco-package
			      (initialize-package stream draco-package))

			(let ((name-string (string name)))
			     (aver (string name-string))

			     (add-comment stream
				(concatenate 'string
					"initialize implementation "
					name-string))

			     (add-package-commands stream
				(translate-commands _initialize-once))

			     (add-comment stream
				(concatenate 'string
					"invoke implementation "
					name-string))))

		(invoke-implementation stream implementation))


	 (add-package-commands (stream commands)
		"invoke several commands"
		(aver (stream stream)
		      (commands commands))
		(map nil
		     #'(lambda (command)
			 (aver (command command))
			 (add-package-command stream (svref command 0)))
		     commands)
		(values))


	 (begin-script (stream title description documentation)
		"write a script header"
		(aver (stream stream)
		      (moniker title)
		      ((optional string) description documentation))
		(format stream *script-header-format*
			title description documentation (date) *version*)
		(values))


	 (find-input (file-type)
		"return list of implementation input files"
		(aver (file-type file-type))
		(mapcar #'file-name
			(if (find file-type ad-hoc-signature)
			    (then (find-every
					#'(lambda (file-datum)
						(aver (file-datum file-datum))
						(eq file-type
						    (file-type file-datum)))
					ad-hoc-file-data))
			    (else (file-data file-type)))))


	 (initialize-package (stream draco-package)
		"invoke draco-package"
		(aver (stream stream)
		      (draco-package draco-package))
		(with-accessors ((name name)
				 (_initialize _initialize)) draco-package
			(add-comment stream
				(concatenate 'string
					"initialize package " (string name)))
			(add-command stream
				(concatenate 'string
					"PACKAGE_INPUT=\""
					(script-tempnam root-name)
					"\""))
			(add-package-commands stream
				(translate-commands _initialize))))


	 (invoke-implementation (stream implementation)
		"actual implementation invocation"
		(aver (stream stream)
		      (implementation implementation))
		(with-accessors ((name name)
				 (input input)
				 (_initialize _initialize)) implementation
			(let* ((init-cmds (translate-commands _initialize))
			       (arg-lists (make-arg-lists implementation))
			       (new-files nil)
			       (num-arg-lists (length arg-lists))
			       (num-init-cmds (cond ((plusp (length init-cmds))
						     (length
							(svref init-cmds 0)))
						    (t 0))))
			      (aver (commands init-cmds)
				    (list arg-lists new-files)
				    (fixnum num-arg-lists num-init-cmds))

			      (cond ((= num-init-cmds num-arg-lists)
				     (dotimes (loop-index num-init-cmds)
					      (aver (fixnum loop-index))
					      (let ((invoke-fn (make-invoke-fn
								 stream
								 loop-index)))
						   (aver (function invoke-fn))
						   (map nil #'invoke-fn
							init-cmds)
						   (setq new-files
							 (nconc new-files
								(apply name
								  stream
								  (nth
								    loop-index
								    arg-lists)
								))))))
				    ((< num-init-cmds 2)
				     (dotimes (loop-index num-arg-lists)
					      (aver (fixnum num-arg-lists))
					      (when (= num-init-cmds 1)
						    (add-package-commands
							stream init-cmds))
					      (setq new-files
						    (nconc new-files
							   (apply name
							     stream
							     (nth loop-index
								  arg-lists)
							   )))))
				    (t (draco-error *cannot-distribute-format*
						    name)))
			      new-files)))


	 (invoke-package (stream implementation)
		"invoke implementation's draco-package"
		(aver (stream stream)
		      (implementation implementation))
		(with-accessors ((draco-package draco-package)) implementation
		  (when draco-package
			(with-accessors ((name name)
					 (_exit _exit)
					 (_pre-invoke _pre-invoke)
					 (_invoke _invoke)) draco-package
				(add-comment stream
					(concatenate 'string
						"invoke package "
						(string name)))
				(add-package-commands stream
					(translate-commands _exit))
				(add-package-commands stream
					(translate-commands _pre-invoke))
				(add-command stream
					(concatenate 'string
					  (svref (translate-command _invoke) 0)
					  " < $PACKAGE_INPUT > /dev/null"))
				(add-command stream "# rm $PACKAGE_INPUT"))))
		(values))


	 (make-arg-lists (implementation)
		"
		returns argument lists for the implementation's Common
		Lisp function using ad-hoc-file-data when possible"
		(aver (implementation implementation))

		(with-accessors ((name name)
				 (primitive primitive)
				 (input input)) implementation
		  (let* ((reconcile-types (mapcar #'cdr
					    (reconciled-input primitive)))
			 (reconcile-types1 reconcile-types)
			 (arg-lists nil)
			 (conj-input nil)
			 (dist-input nil)
			 (nil-input nil))
			(aver (cons reconcile-types reconcile-types1)
			      (list arg-lists conj-input dist-input nil-input))

			;; initialize dist-input and nil-input
			(dolist (file-type (coerce input 'list))
				(aver (file-type file-type))
				(ecase (pop reconcile-types1)
				  (:conjunctive	(push (find-input file-type)
						      conj-input))
				  (:disjunctive	(push (find-input file-type)
						      dist-input))
				  ((nil)	(push (find-input file-type)
						      nil-input))
				))

			(when (or (some #'null conj-input)
				  (some #'null nil-input))
			      (draco-error *too-little-data-format* name))

			(when nil-input
			      (unless (and (reduce #'= nil-input :key #'length)
					   (= 1 (length (first nil-input))))
				      (draco-error
					*too-much-input-format* name)))

			(when dist-input
			      (unless (reduce #'= dist-input :key #'length)
				      (draco-error
					*cannot-distribute-format* name)))

			(dotimes (index (if dist-input
					    (then (length (first dist-input)))
					    (else 1)))
			   (aver (fixnum index))
			   (let ((arg-list nil)
				 (conj-input conj-input)
				 (dist-input dist-input)
				 (nil-input nil-input))
				(aver (list arg-list conj-input dist-input
					    nil-input))
				(dolist (r-type (reverse reconcile-types))
					(aver (reconcile-type r-type))
					(ecase r-type
					  (:conjunctive
						(push (pop conj-input)
						      arg-list))
					  (:disjunctive
						(push (nth index
							   (pop dist-input))
						      arg-list))
					  ((nil)
						(push (pop nil-input)
						      arg-list))
					))
				(push arg-list arg-lists)))

			arg-lists)))


	 (make-conjunctive-string (pathnames)
		"
		if pathnames is a single pathname, its namestring is returned;
		otherwise, a quoted string of namestrings is returned."
		(aver (list pathnames))
		(let ((first-pathname (first pathnames))
		      (rest-pathnames (rest pathnames)))
		     (aver (pathname first-pathname)
			   (list rest-pathnames))
		     (if rest-pathnames
			 (then (format nil "\"~a~{ ~a~}\""
				       first-pathname rest-pathnames))
			 (else (namestring first-pathname)))))


	 (make-invoke-fn (stream fixnum)
		"
		returns a command-invocation function
		for #'invoke-implementation"
		(aver (stream stream)
		      (fixnum fixnum))
		#'(lambda (simple-vector)
			  (aver (simple-vector simple-vector))
			  (add-package-command
				stream (svref simple-vector fixnum))))


	 (make-user-data-string (user-data loop-index)
		"returns the command substring corresponding to user-data"
		(aver (user-data user-data)
		      (fixnum loop-index))
		(if (input user-data)
		    (then (let ((input (mapcar #'file-name
					       (file-data
						 (file-type user-data)))))
			       (aver (list input))
			       (ecase (intern (string-upcase
						(reconcile user-data))
					      'Draco)
				 ((nil)		(namestring (first input)))
				 (conjunctive	(make-conjunctive-string
							input))
				 (disjunctive	(namestring
						  (nth loop-index input)))
			       )))
		    (else (script-tempnam root-name))))


	 (translate-command (command)
		"
		translates command according to the actual data.
		if the translation fails, an error is signaled.

		translation is a two-step process.  first, the data-
		independent input command is translated into a data-
		dependent command by populating all user-data slots.
		then, the data-dependent command is expanded into
		a vector of strings which is then returned."
		(aver (command command))
		(let ((user-data-list nil)
		      (inputs nil)
		      (dist-inputs nil)
		      (nil-inputs nil)
		      (strings nil))
		     (aver (list user-data-list inputs dist-inputs nil-inputs
				 strings))
		     ;; initialize user-data-list
		     (map nil
			  #'(lambda (command-element)
				(aver (command-element command-element))
				(when (typep command-element 'user-data)
				      (push command-element user-data-list)))
			  command)
		     ;; initialize inputs
		     (map nil
			  #'(lambda (user-data)
				    (aver (user-data user-data))
				    (when (input user-data)
					  (push user-data inputs)))
			  user-data-list)
		     ;; initialize nil-inputs and dist-inputs
		     (map nil
			  #'(lambda (input)
				(aver (user-data input))
				(setf (files input)
				      (mapcar #'file-name
					      (find-input (file-type input))))
				(setf (number-of input)
				      (length (files input)))
				(ecase (intern (string-upcase
							(reconcile input)))
				  ((nil)	(push input nil-inputs))
				  (disjunctive	(push input dist-inputs))
				  (conjunctive	)
				))
			  inputs)
		     (unless (every #'plusp (map 'list #'number-of inputs))
			     (draco-error *too-little-data-format* command))
		     (when nil-inputs
			   (unless (and (reduce #'= nil-inputs
						:key #'number-of)
					(= 1 (number-of (first nil-inputs))))
				   (draco-error
					*too-much-input-format* command)))
		     (when dist-inputs
			   (unless (reduce #'= dist-inputs :key #'number-of)
				   (draco-error
					*cannot-distribute-format* command)))
		     (dotimes (loop-index (cond (dist-inputs
						 (number-of
							(first dist-inputs)))
						((plusp (length command)) 1)
						(t 0)))
			      (aver (fixnum loop-index))
			      (push (apply
				      #'concatenate
				      'string
				      (map 'list
					   #'(lambda (element)
						(aver (command-element element))
						(etypecase element
						  (string    element)
						  (symbol    "$LOG_FILE")
						  (user-data (make-user-data-string
								element
								loop-index))
						))
					   command))
				    strings))
		     (apply #'vector strings)))


	 (translate-commands (commands)
		"translates commands according to the actual data"
		(aver (commands commands))
		(map 'commands #'translate-command commands)))


	;; local declarations
	(declare (function add-create-log-file (stream file-spec))
		 (function add-created-files-message (stream string sequence))
		 (function add-implementation (stream implementation) list)
		 (function add-package-commands (stream commands))
		 (function begin-script
			   (stream moniker (optional string) (optional string)))
		 (function find-input (file-type) list)
		 (function initialize-package (stream draco-package))
		 (function invoke-implementation (stream implementation) list)
		 (function invoke-package (stream implementation))
		 (function make-arg-lists (implementation) list)
		 (function make-conjunctive-string (list) string)
		 (function make-invoke-fn (stream fixnum) function)
		 (function make-user-data-string (user-data fixnum) string)
		 (function translate-command (command) command)
		 (function translate-commands (commands) commands))


	(defmethod start-function ((primitive primitive) input documentation)
	  "general-purpose procedural primitive #'start body"
	  (aver (list input)
		((optional string) documentation))

	  (set-data-directory)	; in case the user changed directories

	  ;; put input into canonical form (vector pathname)
	  (setq input
		(typed-vector 'pathname
			(etypecase input
			  (pathname	input)
			  (string	(pathname input))
			  (list		(mapcar #'pathname input))
			)
			:inhibit-checking t))

	  (setq ad-hoc-file-data	(map 'list #'find-file-datum input)
		ad-hoc-signature	(signature ad-hoc-file-data))

	  ;; get ready to create files
	  (setq	root-name
		(concatenate 'string
			"Draco" (date :format "~d~h")
			(subseq (date :format "~y") 2)))

	  (with-accessors ((name name)
			   (doc-string doc-string)) primitive
	    (let ((SIVs (find-primitive-SIVs primitive ad-hoc-signature))
		  (new-files nil)	; initial values
		  (script-name ""))	; not used
		 (aver (list SIVs new-files)
		       (string script-name))

		 (unless SIVs (draco-error *no-implementations-format* name))

		 ; pick an SIV, any SIV . . .
		 (dolist (impl (coerce (impl-vector (first SIVs)) 'list))
			 (aver (implementation impl))

			 (setq	script-name
				(namestring
				  (make-pathname
				    :defaults	(script-tempnam root-name)
				    :type	*script-type*)))

			 (draco-inform *generating-script-format*
				(file-namestring script-name) (name impl))

			 (with-open-file (stream script-name
					  :direction :output
					  :if-does-not-exist :create)
					 (aver (stream stream))
				(begin-script stream
					(name (primitive impl))
					(doc-string (primitive impl))
					documentation)
				(add-create-log-file stream
				  (make-pathname
					:defaults	script-name
					:type		*log-file-type*))
				(setq new-files
				      (add-implementation stream impl))
				(invoke-package stream impl)
				(add-created-files-message
					stream script-name new-files))

			 (draco-inform *invoking-script-format*
				(file-namestring script-name) (name impl))

			 (ignore-errors
				(shell (concatenate 'string
						"chmod +x " script-name)
					:output			#p"/dev/null"
					:if-output-exists	:append
					:error 			:output))

			 (shell script-name :ignore-status t)	; hang on . . .

			 (dolist (file-spec new-files)
				 (aver (file-spec file-spec))
				 (push (find-file-datum file-spec)
				       ad-hoc-file-data))

			 (setq ad-hoc-signature (signature ad-hoc-file-data))

			 (update-inventory))))
	  (values))

      ) ; labels
) ; let*


(defmethod input ((primitive primitive))
	"extract the input (data-type) vector from the reconciled-input slot"
	(map '(vector data-type) #'car (reconciled-input primitive)))


(defmethod make-start-function ((primitive primitive))
	"create package and #'start for procedural primitive"
	(with-accessors ((name name)
			 (reconciled-input reconciled-input)) primitive
		(let* ((package-name (string-upcase name))
		       (package (or (find-package package-name)
				    (make-package package-name
					:use '("LISP" "DRACO"))))
		       (start-symbol (intern "START" package)))
		      (aver (string package-name)
			    (package package)
			    (moniker start-symbol))
		      (export start-symbol package)
		      (eval `(declaim (function ,start-symbol
						(list &key documentation))))
		      (compile start-symbol
			`(lambda (input &key (documentation nil))
				 "generated by Draco"
				 (aver (list input)
				       ((optional string) documentation))
				 (draco:start-function
					,primitive input documentation)))))
	(values))


;; internal function definitions
(defun find-SIVs (primitives signature)
	"
	returns a list of SIV structures corresponding to primitives.
	does not know about file-type conversion!"
	(aver (list primitives)
	      (signature signature))
	(let ((SIVs (and primitives
			 (find-primitive-SIVs (pop primitives) signature))))
	     (aver (list SIVs))
	     (if primitives
		 (then (mapcan
			 #'(lambda (SIV)
				   (aver (SIV SIV))
				   (mapcar #'(lambda (rest-SIV)
						     (aver (SIV rest-SIV))
						     (merge-SIVs SIV rest-SIV))
					   (find-SIVs primitives
						      (signature SIV))))
			 SIVs))
		 (else SIVs))))      


(defun make-reconciled-input-alist (reconciled-input-spec)
	"return alist corresponding to user's reconciled-input specification"
	(aver ((or list symbol) reconciled-input-spec))
	(let ((specs (mapcar #'(lambda (spec)
				 (aver ((or cons symbol) spec))
				 (etypecase spec
				   (cons	spec)
				   (symbol	(list spec))
				 ))
			     (if (listp reconciled-input-spec)
				 (then reconciled-input-spec)
				 (else (list reconciled-input-spec))))))
	     (aver (list specs))
	     (mapcar #'cons
		(mapcar #'(lambda (spec)
				  (aver (cons spec))
				  (get-instance 'data-type :name (car spec)))
			specs)
		(mapcar #'cadr specs))))


(defun make-SIV (impl-vector signature)
	"create and return an instance of SIV"
	(aver (impl-vector impl-vector)
	      (signature signature))
	(make-instance 'SIV
		:documentation	*SIV-documentation*
		:impl-vector	impl-vector
		:signature	signature))


(defun merge-SIVs (&rest args)
	"merges SIV arguments into a single SIV instance"
	(aver (list args))
	(make-SIV
	  (apply #'concatenate 'impl-vector (mapcar #'impl-vector args))
	  (apply #'merge-signatures (mapcar #'signature args))))


;; exported function definitions
(defun add-command (stream command)
	"add command (string) to (end of) script"
	(aver (stream stream)
	      (string command))
	(format stream *add-command-format* command)
	(values))


(defun add-comment (stream comment)
	"add comment to (end of) script"
	(aver (stream stream)
	      (string comment))
	(format stream *add-comment-format* comment)
	(values))


(labels
  ((translate-quotes (command)
	"prefix double-quotes in command with backslashes"
	(aver (string command))
	(let ((quote (position #\" command :test #'char-equal)))
	     (aver ((optional fixnum) quote))
	     (when quote
		   (setq command
			 (concatenate 'string
				(subseq command 0 quote)
				"\\\""
				(translate-quotes
					(subseq command (1+ quote)))))))
	command))

  (declare (function translate-quotes (string) string))

  (defun add-package-command (stream command-string)
	"add a package command"
	(aver (stream stream)
	      (string command-string))
	(format stream *add-command-format*
		(concatenate 'string
			"echo "
			(translate-quotes command-string)
			" >> $PACKAGE_INPUT"))
	(values))
) ; labels


(defun script-tempnam (&optional (prefix "Draco"))
	"return name of \"temporary\" file in current directory"
	(aver (string prefix))
	(tempnam :directory	(namestring (cwd))
		 :prefix	prefix
		 :tag		'Draco))
