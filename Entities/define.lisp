;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	define-<entity> macros
;;;

(in-package draco)

#!(export '(define-converter		define-data-type
	    define-file-datum		define-file-type	
	    define-implementation	define-initialization-file
	    define-KV-reporter		define-package
	    define-primitive		define-procedure))


;; internal macro definitions
(defmacro define-internal-data-type (&rest args)
	  "create and return a system-defined data-type"
	  `#!(let ((data-type (define-data-type ,@args)))
		  (aver (data-type data-type))
		  (setf (defined-by data-type) (list *Draco-package*))
		  data-type))


(defmacro define-internal-file-type (&rest args)
	  "create and return a system-defined file-type"
	  `#!(let ((file-type (define-file-type ,@args)))
		  (aver (file-type file-type))
		  (setf (defined-by file-type) (list *Draco-package*))
		  file-type))


(defmacro define-internal-implementation (&rest args)
	  "create and return a system-defined implementation"
	  `#!(let ((implementation (define-implementation ,@args)))
		  (aver (implementation implementation))
		  (setf (defined-by implementation) (list *Draco-package*))
		  implementation))


(defmacro define-internal-initialization-file (&rest args)
	  "create and return a system-defined initialization-file"
	  `#!(let ((init-file (define-initialization-file ,@args)))
		  (aver (initialization-file init-file))
		  (setf (defined-by init-file) (list *Draco-package*))
		  init-file))


(defmacro define-internal-package (&rest args)
	  "create and return a system-defined package"
	  `#!(let ((draco-package (define-package ,@args)))
		  (aver (draco-package draco-package))
		  (setf (defined-by draco-package) (list *Draco-package*))
		  draco-package))


(defmacro define-internal-primitive (&rest args)
	  "create and return a system-defined primitive"
	  `#!(let ((primitive (define-primitive ,@args)))
		  (aver (primitive primitive))
		  (setf (defined-by primitive) (list *Draco-package*))
		  primitive))


(defmacro define-internal-procedure (&rest args)
	  "create and return a system-defined procedure"
	  `#!(let ((procedure (define-procedure ,@args)))
		  (aver (primitive procedure))
		  (setf (defined-by procedure) (list *Draco-package*))
		  procedure))


;; exported macro definitions
(defmacro define-converter
	  (&key (name		(draco-error *missing-keyword-format*
				  "define-converter" :name))
		(documentation	nil)
		(input		(draco-error *missing-keyword-format*
				  "define-converter" :input))
		(output		(draco-error *missing-keyword-format*
				  "define-converter" :output))
		(package	nil)
		(syntax		(draco-error *missing-keyword-format*
				  "define-converter" :syntax)))
	"create and return a Draco (data format) converter"
	`#!(let ((input (get-instance 'file-type :name ',input))
		 (output (get-instance 'file-type :name ',output)))
		(aver (file-type input output))
		(get-instance 'converter
			:name		',name
			:documentation	',documentation
			:input		input
			:output		output
			:draco-package	(and ',package
					     (get-instance 'draco-package
						:name ',package))
			:syntax		(typed-vector 'string ',syntax)
			:_syntax	(translate-cmd-strings
					  ',syntax
					  (typed-vector 'file-type input)
					  (typed-vector 'file-type output)))))


(defmacro define-data-type
	  (&key (name		(draco-error *missing-keyword-format*
				  "define-data-type" :name))
		(documentation	nil)
		(file-types	(draco-error *missing-keyword-format*
				  "define-data-type" :file-types)))
	"create and return a Draco data-type"
	`#!(get-instance 'data-type
		:name		',name
		:documentation	',documentation
		:file-types	(get-instance-vector 'file-type ',file-types)))


(defmacro define-file-datum
	  (&key (file-name	(draco-error *missing-keyword-format*
				  "define-file-datum" :file-name))
		(file-type	(draco-error *missing-keyword-format*
				  "define-file-datum" :file-type))
		(KV-pairs	nil)
		(description	""))
	"create and return a Draco file-datum"
	`#!(let* ((pairs (mapcar #'(lambda (pair) (apply #'cons pair))
				 ',KV-pairs))
		  (file-type (get-instance 'file-type :name ',file-type))
		  (file-datum (get-file-datum ',file-name 
				:file-type	file-type
				:KV-pairs	pairs
				:description	',description)))
		 (aver (list pairs)
		       (file-type file-type)
		       (file-datum file-datum))
		 (pushnew file-datum (file-data file-type))
		 file-datum))


(defmacro define-file-type
	  (&key (name		(draco-error *missing-keyword-format*
				  "define-file-type" :name))
		(documentation	nil)
		(super-type	nil)
		(recognizer	(draco-error *missing-keyword-format*
				  "define-file-type" :recognizer))
		(reporter	'*default-reporter*)
		(reporter-type	'*default-reporter-type*))
	"
	create and return a Draco file type; for documentation,
	see Samples;file-types (sorry)"
	`#!(let ((reporter ,reporter))
		(aver ((or KV-reporter symbol) reporter))
		(get-instance 'file-type
			:name		',name
			:documentation	',documentation
			:super-type	(and ',super-type
					     (get-instance 'file-type
						:name	',super-type))
			:recognizer	(typecase ,recognizer
					  (string (pathname ,recognizer))
					  (t	  ,recognizer)
					)
			:reporter	(typecase reporter
					  (null	  nil)
					  (symbol (fdefinition reporter))
					  (t	  reporter)
					)
			:reporter-type	,reporter-type)))


(defmacro define-implementation
	  (&key (name			(draco-error *missing-keyword-format*
					  "define-implementation" :name))
		(documentation		nil)
		(package		nil)
		(input			(draco-error *missing-keyword-format*
					  "define-implementation" :input))
		(output			nil)
		(initialize		nil)
		(initialize-once	nil))
	"create and return a Draco implementation"
	`#!(get-instance 'implementation
		:name			',name
	  	:documentation		',documentation
		:draco-package		(and ',package
					     (get-instance 'draco-package
						:name ',package))
		:input			(get-instance-vector 'file-type
						',input)
		:output			(get-instance-vector 'file-type
						',output)
	  	:initialize		(typed-vector 'string ',initialize)
	  	:initialize-once	(typed-vector 'string
						',initialize-once)))


(defmacro define-initialization-file
	  (&key (name			nil)
		(documentation		nil)
		(file			(draco-error *missing-keyword-format*
					  "define-initialization-file" :file))
		(load-immediately	nil)
		(update-fn		nil)
		(update-parameters	nil))
	"register a data file, e.g. Samples;file-types"
	;; the :fire slot gets initialized via a shared-initialize method
	`#!(get-instance 'initialization-file
		:name			(or ',name (intern ,file))
		:documentation		',documentation
		:file			,file
		:load-immediately	,load-immediately
		:update-fn		,update-fn
		:update-parameters	,update-parameters))


(defmacro define-KV-reporter
	  (&key (name			(draco-error *missing-keyword-format*
					  "define-KV-reporter" :name))
		(documentation		nil)
		(lex-function		(draco-error *missing-keyword-format*
					  "define-KV-reporter" :lex-function))
		(reporter-type		:f-sorted)
		(report-keywords	nil report-keywords-supplied-p))
	"define a keyword-value file reporter"
	`#!(let ((kv-reporter
		   (get-instance 'KV-reporter
			:name		',name
			:documentation	',documentation
			:lex-function	(typecase ',lex-function
					  (string	(pathname
							  ',lex-function))
					  (symbol	',lex-function)
					  (t		,lex-function)
					)
			:reporter-type	',reporter-type)))
		(aver (KV-reporter kv-reporter))
		(when (or ,report-keywords-supplied-p
			  (not (slot-boundp kv-reporter 'report-keywords)))
		      (define-report-keywords
			:reporter	,name
			:keywords	,report-keywords))
		kv-reporter))


(defmacro define-package
	  (&key (name		(draco-error *missing-keyword-format*
				  "define-package" :name))
		(documentation	nil)
		(file-type	nil)
		(pre-invoke	nil)
		(invoke		"")
		(initialize	nil)
		(exit		nil))
	"create and return a Draco (analysis) package"
	`#!(get-instance 'draco-package
		:name		',name
		:documentation	',documentation
		:file-type	(and ',file-type
				     (get-instance 'file-type
					:name	',file-type))
		:pre-invoke	(typed-vector 'string ',pre-invoke)
		:invoke		',invoke
		:initialize	(typed-vector 'string ',initialize)
		:exit		(typed-vector 'string ',exit)
		:_pre-invoke	(translate-cmd-strings ',pre-invoke)
		:_invoke	(translate-cmd-string ',invoke)
		:_initialize	(translate-cmd-strings ',initialize)
		:_exit		(translate-cmd-strings ',exit)))


(defmacro define-primitive
	  (&key (name			(draco-error *missing-keyword-format*
					  "define-primitive" :name))
		(documentation		nil)
		(reconciled-input	(draco-error *missing-keyword-format*
					  "define-primitive" :reconciled-input))
		(output			nil)
		(primitives		nil)
		(implementations	nil))
	"create and return a Draco primitive"
	`#!(get-instance 'primitive
		:name			',name
		:documentation		',documentation
		:reconciled-input	(make-reconciled-input-alist
						',reconciled-input)
		:output			(get-instance-vector 'data-type
						',output)
		:primitives		(get-instance-vector
						'primitive ',primitives)
		:implementations	(get-instance-vector 'implementation
						',implementations)))


(defmacro define-procedure
	  (&key (name		(draco-error *missing-keyword-format*
				  "define-procedure" :name))
		(documentation	nil)
		(primitives	(draco-error *missing-keyword-format*
				  "define-procedure" :primitives)))
	"create and return a Draco procedure"
	`#!(get-instance 'primitive
		:name		',name
		:documentation	',documentation
		:procedure	t
		:primitives	(get-instance-vector 'primitive ',primitives)))
