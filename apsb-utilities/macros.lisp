;;; -*- Mode: Lisp; Package:(apsb-utilities)-*-
;;;
;;;	macros definitions

(in-package apsb-utilities)

;; first define any read macros
(eval-when (:compile-toplevel :load-toplevel :execute)
	(defun |#!| (stream subchar arg)
		"
		Reader function for the dispatch macro #!.  #!(foo) is the same
		as (eval-when (:compile-toplevel :load-toplevel :execute) foo)"
		(declare (ignore subchar arg))
		`(eval-when (:compile-toplevel :load-toplevel :execute)
			,(read stream nil nil t)))

	(set-dispatch-macro-character #\# #\! #'|#!|)
	#+:Allegro (tpl:setq-default *readtable* *readtable*)	; not a NOP!
)
;; we return you to your regularly scheduled program


#!(export '(*aver* auto-doc-format aver else then))


;; special variable
(defparameter
	*aver*
	t
	"t -> aver translates to check-type statements; nil -> aver = nop"
)


;; declaration
(declaim (t *aver*))


;; exported macro definitions
(defmacro auto-doc-format (key-spec call-syntax &body body)
	"
	syntax:	(auto-doc-format <key-spec> <call-syntax>
		  [:map <map-name>]
		  [form]*
		)

	key-spec may be a symbol or a list of symbols.  map-name defaults
	to '*auto-doc-default-map*.  if forms are specified, this macro
	creates a function from them and uses it to format forms beginning
	with a key-spec symbol.  in this case, call-syntax becomes part
	of the format function's documentation string.  if no forms are
	specified, no function is created and forms beginning with a
	key-spec symbol are ignored.

	the following functions may be used to define format functions:

		format-declarations	format-documentation
		format-define-function	format-form-type
		format-define-variable	format-lambda-list
		format-distinguish	format-new-entry"

	(let ((forms (gensym))
	      (map-name (gensym)))
	     `#!(when (find :apsb-auto-doc *features*)
		      (let ((,map-name '*auto-doc-default-map*)
			    (,forms ',body))
			   (aver (symbol ,map-name)
				 (list ,forms))
			   (when (and (symbolp (first ',body))
				      (string= "MAP"
					       (symbol-name (first ',body))))
				 (setq	,map-name	(second ',body)
					,forms		(cddr ',body)))
			   (auto-doc-format-fn
				',key-spec ',call-syntax ,forms ,map-name)))))


(defmacro aver (&rest clauses)
	"dynamically check type declarations when *aver* is t"
	(declare (cons clauses))
	`#!(when *aver*
		 (progn
		   ,@(mapcan
			#'(lambda (clause)
			    (declare (cons clause))
			    (when (and (symbolp (first clause))
				       (string= "TYPE" (first clause)))
				  (pop clause))
			    (mapcar
				#'(lambda (symbol)
				    (declare (symbol symbol))
				    (list 'check-type symbol (first clause)))
				(rest clause)))
			clauses))))


(defmacro define-test-function (function-name &body body)
	"
	syntax:	(define-test-function <function-name>
		  [:map <map-name>]
		  [:key <key-name>]
		  [:test <test-name>]
		  [:let <bindings>]
		  [documentation]
		  [(<form> <values>)]*
		)

	map-name defaults to '*test-default-map* and test defaults to
	#'equal.  each <form> should return the corresponding <values>"
	`#!(when (find :apsb-test *features*)
		 (funcall 'define-test-function-fn ',function-name ',body)))


(defmacro else (&rest stmts)
	"conditional syntactic sugar"
	(aver (cons stmts))
	`#!(progn ,@stmts))


(defmacro then (&rest stmts)
	"conditional syntactic sugar"
	(aver (cons stmts))
	`#!(progn ,@stmts))
