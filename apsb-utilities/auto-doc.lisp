;;; -*- Mode:Common-Lisp; Package:(apsb-utilities); Base:10 -*-
;;;
;;;	generate documentation for lisp code
;;;
;;;	Remarks:
;;;	- based on Mark Johnston's quick-doc and Jeff Sponsler's def-tables
;;;
;;;	Known bugs & limitations:
;;;	- only supports cons forms (quick-doc also printed strings)
;;;	- it would be nice if it ignored RCS cruft
;;;
;;;	Author (Date):	fwy (may92)
;;;	Key Gadfly:	mrose
;;;

(in-package apsb-utilities)

#!(export '(*auto-doc-default-map* *auto-doc-mystery-types* auto-doc
	    auto-doc-dir auto-doc-form auto-doc-format-fn form-declarations
	    format-declarations format-define-function format-define-variable
	    format-distinguish format-documentation format-form-type
	    format-lambda-list format-new-entry))


;; type
(deftype last-printed ()
	"
	indicates what auto-doc printed most recently.
	nil -> don't know; :comment -> comment; :form -> form"
	'(member nil :comment :form)
)


;; constants (duh!)
(defconstant	*auto-doc-default-extension*
	"adoc"
	"default extension for an auto-doc file"
)
;; format strings
(defconstant	*auto-doc-begin-format*
	"Auto-Doc for: ~a (~a)"
	"arguments: file name, date"
)
(defconstant	*auto-doc-end-format*
	"~%~%End of Auto-Doc for: ~a~%"
	"argument: file name"
)
(defconstant	*auto-doc-key-entered-format*
	"~&auto-doc: ~:[ignoring~;recognizing~] ~a (~a)~%"
	"arguments: function name, key, map name"
)
(defconstant	*format-declarations-format*
	"~{~%  ~a~}"
	"argument: declarations"
)
(defconstant	*format-documentation-format*
	"~@[~%  ~a~]"
	"argument: doc-string"
)
(defconstant	*format-form-type-format*
	"~@[ [~a~@[ (eval-when ~a)~]]~]"
	"arguments: form type, eval-when context"
)
(defconstant	*format-lambda-list-format*
	"~%   ~:s"
	"argument: lambda list"
)
(defconstant	*format-new-entry-format*
	"~%~%~a"
	"argument: entry name"
)


;; special variables
(defparameter	*auto-doc-default-map*
	nil
	"
	alist mapping cons forms to format functions.  a null function
	entry indicates that the form should be ignored.  all format
	functions accept only the following keyword arguments:
		:form  :stream  :verbose  :eval-when  :map
	use auto-doc-format to create table entries and their
	corresponding format functions."
)
(defvar		*auto-doc-mystery-types*
	nil
	"(first form) for each unrecognized cons, no duplicates"
)


;; declarations
(declaim (string *auto-doc-key-entered-format* *format-declarations-format*
		 *format-documentation-format* *format-form-type-format*
		 *format-form-type-format* *format-lambda-list-format*
		 *format-new-entry-format*))
(declaim (list *auto-doc-default-map* *auto-doc-mystery-types*))

(declaim (function auto-doc
		   (string &optional string &key verbose eval-when) list))
(declaim (function auto-doc-dir (&key directory extension verbose) list))
(declaim (function auto-doc-find (list symbol t) t))
(declaim (function auto-doc-form (t &optional stream boolean list symbol)))
(declaim (function auto-doc-format-fn (boolean symbol symbol string list)))
(declaim (function form-declarations (cons) list))
(declaim (function format-declarations (stream list)))
(declaim (function format-define-function
		   (&key name form-type lambda-list decls
			 documentation stream eval-when)))
(declaim (function format-define-variable
		   (&key form form-type documentation stream eval-when)))
(declaim (function format-distinguish
		   (&key name form-type documentation stream eval-when)))
(declaim (function format-documentation (stream (optional string))))
(declaim (function format-form-type (stream (or string symbol) list)))
(declaim (function format-lambda-list (stream list)))
(declaim (function format-new-entry (stream symbol)))

(declaim (inline auto-doc-find		auto-doc-form		; speed kills?
		 format-declarations	format-documentation
		 format-form-type	format-lambda-list
		 format-new-entry))


;; local function definitions
(labels
  ((auto-doc-comment (string outstream last-printed)
	"
	print comment string to outstream.  last-printed may
	be :comment or some other symbol.  Rules here are:
	    1.  only top-level ;;; comments are printed
	    2.  if last-printed was not a comment, then a
		newline is emitted before the comment string."
	(aver (string string)
	      (stream outstream)
	      (last-printed last-printed))
	(when (and (> (length string) 2)
		   (string= string ";;;" :start1 0 :end1 3))
	      (unless (eq last-printed :comment)
		      (terpri outstream))
	      (terpri outstream)
	      (princ (subseq string 3) outstream))
	(values))


   (auto-doc-update-map (key function-name map-name)
	"add (key . function-name) to named map"
	(aver (symbol key function-name map-name))

	(set map-name
	     (acons (symbol-name key) function-name (eval map-name)))

	(eval-when (:execute)
		(when *load-verbose*
		      (format *diagnostic-stream* *auto-doc-key-entered-format*
			      function-name key map-name)))
	(values))


   (format-mystery-cons
     (&key form (stream *standard-output*) (verbose nil) (eval-when nil))
	"
	input is a form (cons) and a stream; a brief description about the
	(unrecognized) form is printed to the stream if verbose is non-nil."
	(declare (ignore stream))
	(aver (cons form)
	      (boolean verbose)
	      (list eval-when))

	(when verbose
	      (format *diagnostic-stream* "~%~%?? ~s ~s~@[ (eval-when ~a)~]~%"
		(first form) (second form) eval-when))
	(pushnew (first form) *auto-doc-mystery-types*)
	(values))


   (make-format-fn (call-syntax body)
	"creates a format function"
	(aver (string call-syntax)
	      (cons body))

	(let ((function-name (gentemp)))
	     (aver (symbol function-name))

	     (eval `(declaim (function ,function-name
				       (&key form stream verbose eval-when))))

	     (setf (documentation function-name 'function)
		   (concatenate 'string
			(format nil "~%format ~a forms.~%~%" call-syntax)
			"for more information, see *auto-doc-default-map*."))
	     (compile function-name
		`(lambda (&key form (stream *standard-output*)
			       (verbose nil) (eval-when nil)
			       (map *auto-doc-default-map*))
			 ; inhibit "variable <var> is never used" warnings
			 (or form stream verbose eval-when map)
			 ,@body))

	     (values function-name)))


   (skip-balanced-comment (stream)
	"
	skip over balanced comment (#|...); handles nested
	balanced comments; returns when the balanced comment
	is terminated (...|#) or if end of file is reached."
	(aver (stream stream))

	(let ((prev-char (read-char stream))	; initially (eq #\| prev-char)
	      (char #\space)			; initialized below
	      (depth 1))
	     (aver (character prev-char char)
		   (fixnum depth))
	     (loop (case (setq char (read-char stream nil :eof))
		     (:eof	(return nil))		; end of file
		     (#\#	(when (and (char= #\| prev-char)
					   (zerop (decf depth)))
				      (return nil)))	; end of comment
		     (#\|	(when (char= #\# prev-char)
				      (incf depth)))	; nested comment
		     (t		))
		   (setq prev-char char)))
	(values)))


  ;; local declarations
  (declare (function auto-doc-comment (string stream last-printed))
	   (function auto-doc-update-map (symbol symbol symbol))
	   (function format-mystery-cons (&key form stream verbose eval-when))
	   (function make-format-fn (string cons) symbol)
	   (function skip-balanced-comment (stream)))


  ;; exported function definitions
  (defun auto-doc (lisp-file &optional (extension *auto-doc-default-extension*)
		   &key (verbose nil) (eval-when nil)
			(map *auto-doc-default-map*))
	"
	Write top-level ;;; comments and documentation extracted from
	a lisp source file and written to a like-named file in the same
	directory with the specified extension.  (all other comments are
	ignored.)  if verbose is specified, unrecognized forms are ignored;
	otherwise, they are formatted by #'format-mystery-cons.  a list of
	unrecognized forms is returned.

	eval-when is used (internally) to track (nested) eval-when contexts.

	for more information, see *auto-doc-mystery-types*."
	(aver (file-spec lisp-file)
	      (string extension)
	      (boolean verbose)
	      (list eval-when map))

	(with-open-file (outstream
				(make-pathname	:defaults	lisp-file
						:type		extension)
				:direction		:output
				:if-does-not-exist	:create
				:if-exists		:supersede)

	  (format outstream *auto-doc-begin-format*
		  (truename lisp-file) (date))
    
	  (with-open-file (stream lisp-file :direction :input)

	    (let ((last-printed nil)
		  (char #\space))		; initialized below
		 (aver (last-printed last-printed)
		       (standard-char char))

		 (loop (case (setq char (read-char stream nil :eof))
			 (:eof	(return nil))
           
			 ((#\newline #\space #\page #\tab #\return #\linefeed))

			 (#\;	(unread-char #\; stream)
				(auto-doc-comment
				  (read-line stream nil :eof)
				  outstream last-printed)
				(setq last-printed :comment))
           
			 (#\#	(if (eql #\| (peek-char nil stream))
				    (then (skip-balanced-comment stream))
				    (else (unread-char #\# stream)
					  (auto-doc-form
					    (read stream nil :eof)
					    outstream verbose eval-when)
					  (setq last-printed :form))))

			 (t	(unread-char char stream)
				(auto-doc-form
				  (read stream nil :eof)
				  outstream verbose eval-when map)
				(setq last-printed :form))))))

	  (format outstream *auto-doc-end-format* (truename lisp-file)))

	(values *auto-doc-mystery-types*))


  (defun auto-doc-dir (&key (directory ".")
			    (extension *auto-doc-default-extension*)
			    (verbose nil))
	"
	run auto-doc on all lisp source files in the specified directory;
	output files have the same root name and the specified extension
	and are created in the current working directory.  returns a list
	of unrecognized forms.

	for more information, see auto-doc, *auto-doc-mystery-types*."
	(aver (string directory extension)
	      (boolean verbose))

	(with-open-stream 
	  (ls-stream (run-shell-command
			(concatenate 'string "ls " directory "/*.lisp")
			:wait nil  :output :stream))
	  (aver (stream ls-stream))

	  (loop (let ((source-file (read-line ls-stream nil nil)))
		     (aver ((optional string) source-file))

		     (when (null source-file) (return))	; eof test

		     (auto-doc source-file extension :verbose verbose))))

	(values *auto-doc-mystery-types*))


  (defun auto-doc-find (keyword-values key &optional (default nil))
	"
	keyword-values is a list of cons'es.  the car of each cons is
	interpreted as a keyword.  auto-doc-find returns the cdr of the
	cons whose keyword matches the specified key.  the optional
	default is returned if no matching keyword was found."
	(aver (list keyword-values)
	      (symbol key)
	      (t default))

	(or (cdr (find key keyword-values :key #'car))
	    default))


  (defun auto-doc-form (form
			&optional (stream *standard-output*)
				  (verbose nil) (eval-when nil)
				  (map *auto-doc-default-map*))
	"
	input is a form and a stream; if the form is one of the recognized
	types, documentation about the form is written to the stream.
	if the form is unrecognized, e.g. if it is not a cons, and verbose
	is t, a brief description is written to the stream.  otherwise,
	nothing is written."
	(aver (t form)
	      (stream stream)
	      (boolean verbose)
	      (list eval-when map))

	(when (consp form)
	      (let ((pair (assoc (symbol-name (car form)) map
				 :test #'string=)))
		   (aver (list pair))
		   (unless (and pair (null (cdr pair)))
			   (funcall (if pair
					(then (symbol-function (cdr pair)))
					(else #'format-mystery-cons))
				:form		form
				:stream		stream
				:verbose	verbose
				:eval-when	eval-when))))
	(values))


  (defun auto-doc-format-fn (key-spec call-syntax forms map-name)
	"does (virtually) everything for auto-doc-format"
	(aver ((or cons moniker) key-spec)
	      (string call-syntax)
	      (list forms)
	      (symbol map-name))

	(let ((function-name (and forms
				  (make-format-fn call-syntax forms))))
	     (aver (symbol function-name))

	     (etypecase key-spec
		(symbol	(auto-doc-update-map key-spec function-name map-name))

		(cons	(dolist (key key-spec)
				(aver (symbol key))
				(auto-doc-update-map
				  key function-name map-name)))))
	(values))


  (defun form-declarations (form)
	"return top-level declarations in form"
	(aver (cons form))

	(find-every #'(lambda (clause)
			(aver (t clause))
			(case (first clause)
			  ((aver declare)	clause)
			  (t			nil)))
		    (remove-if #'atom form)))


  (defun format-declarations (stream decls)
	"format optional declarations"
	(aver (stream stream)
	      (list decls))

	(format stream *format-declarations-format* decls)
	(values))


  (defun format-define-function
	 (&key name form-type lambda-list (decls nil) (documentation nil)
	       (stream *standard-output*) (eval-when nil))
	"format defun and related forms"
	(aver ((or cons symbol) name)
	      (string form-type)
	      (list lambda-list decls)
	      ((optional string) documentation)
	      (stream stream)
	      (list eval-when))

	(format-new-entry stream	name)
	(format-form-type stream	form-type eval-when)
	(format-lambda-list stream	lambda-list)
	(format-documentation stream	documentation)
	(format-declarations stream	decls)
	(values))


  (defun format-define-variable
	 (&key name form-type value documentation
	       (stream *standard-output*) (eval-when nil))
	"format defvar and related forms"
	(aver (symbol name)
	      (string form-type) ; e.g. "constant", "parameter", "variable"
	      (t value)
	      ((optional string) documentation)
	      (stream stream)
	      (list eval-when))

	(format-new-entry stream	name)
	(format-form-type stream	form-type eval-when)
	(format stream "~@[ value: ~s~]" value)
	(format-documentation stream	documentation)
	(values))


  (defun format-distinguish
	 (&key name form-type (documentation nil)
	     (stream *standard-output*) (eval-when nil))
	"
	format forms that distinguish one or more entities,
	e.g. types, exported symbols, etc."
	(aver (t name)
	      (string form-type)
	      ((optional string) documentation)
	      (stream stream)
	      (list eval-when))

	(format-new-entry stream	name)
	(format-form-type stream	form-type eval-when)
	(format-documentation stream	documentation)
	(values))


  (defun format-documentation (stream documentation)
	"format optional documentation strings"
	(aver (stream stream)
	      ((optional string) documentation))

	(format stream *format-documentation-format* documentation)
	(values))


  (defun format-form-type (stream form-type eval-when)
	"format form types and eval-when contexts"
	(aver (stream stream)
	      ((or string symbol) form-type)
	      (list eval-when))

	(format stream *format-form-type-format* form-type eval-when)
	(values))


  (defun format-lambda-list (stream lambda-list)
	"format lambda lists"
	(aver (stream stream)
	      (list lambda-list))

	(format stream *format-lambda-list-format* lambda-list)
	(values))


  (defun format-new-entry (stream name)
	"start formatting a new entry"
	(aver (stream stream)
	      (t name))

	(format stream *format-new-entry-format* name)
	(values))
) ; labels
