;;; -*- Mode:Common-Lisp; Package:(apsb-utilities); Base:10 -*-
;;;
;;;	initialize auto-doc utility
;;;

(in-package apsb-utilities)


(auto-doc-format auto-doc-format
  "(auto-doc-format <key-spec> <call-syntax> <body>)"
  ;; null function body means that the form is ignored
)


(auto-doc-format (aver declaim declare proclaim)
  "(<declare-op> {decl}*)"
  ;; null function body means that the form is ignored
)


(auto-doc-format component-operation
  "(component-operation <oos-keyword> <function>)"

  (format-new-entry stream	(second form))
  (format-form-type stream	"make:operate-on-system operation" eval-when)
  (format stream "~%  function: ~a" (third form))
)


(auto-doc-format (do do* dolist dotimes mapcar)
  "(<do-op> ...)"
  ;; null function body means that the form is ignored
)


(auto-doc-format defclass
  "(defclass <name> ({superclass-name>}*) ({slot-specifier}*) {class-option}*)"

  (format-new-entry stream	(second form))
  (format-form-type stream	"class" eval-when)
  (format stream "~@[~%  parent classes: ~a~]" (third form))
  (format-documentation stream	(car (auto-doc-find
					(cddddr form) :documentation)))

  (if (fourth form)
      (then
	(format stream "~%  slots:")
	(dolist (slot (fourth form))
		(aver (cons slot))
		(format stream
			"~%   ~a ~@[  accessor: ~a~]"
			(first slot)
			(getf (rest slot) :accessor))
		(format stream
			"~@[  reader: ~a~]~@[  writer: ~a~]~@[~%     ~a~]"
			(getf (rest slot) :reader)
			(getf (rest slot) :writer)
			(getf (rest slot) :documentation)))))
  (values))


(auto-doc-format defconstant
  "(defconstant <name> <initial-value> [documentation])"

  (format-define-variable
	:name		(second form)
	:form-type	"constant"
	:value		(third form)
	:documentation	(fourth form)
	:stream		stream
	:eval-when	eval-when))


(auto-doc-format defcstruct
  "(defcstruct <name> {slot}+)"
  ;; null function body means that the form is ignored
)


(auto-doc-format (defforeign defforeign-list)
  "(<defforeign-op> ...)"
  ;; null function body means that the form is ignored
)


(auto-doc-format defgeneric
  "(defgeneric <name> <lambda-list> {option|method-description}*)"

  (format-define-function
	:name		(second form)
	:form-type	"generic function"
	:lambda-list	(third form)
	:decls		(auto-doc-find (cdddr form) 'declare)
	:documentation	(car (auto-doc-find (cdddr form) :documentation))
	:stream		stream
	:eval-when	eval-when))


(auto-doc-format defmacro
  "(defmacro <name> <lambda-list> [<decls>|<doc-string>] {form}*)"

  (format-define-function
	:name		(second form)
	:form-type	"macro"
	:lambda-list	(third form)
	:decls		(form-declarations form)
	:documentation	(find-if #'stringp (butlast (cdddr form)))
	:stream		stream
	:eval-when	eval-when))


(auto-doc-format defmethod
  "(defmethod <name> {qualifier}* <lambda-list> [<decls>|<doc-string>] {form}*)"

  (let* ((lambda-body (member-if #'listp form))
	 (qualifiers (cddr (ldiff form lambda-body))))
 	(aver (cons lambda-body)
	      ((or list symbol) qualifiers))

	(unless (cdr qualifiers)
		(setq qualifiers (car qualifiers)))

	(format-define-function
		:name		(second form)
		:form-type	(format nil "~@[~a ~]method" qualifiers)
		:lambda-list	(first lambda-body)
		:decls		(form-declarations lambda-body)
		:documentation	(find-if #'stringp (butlast (rest lambda-body)))
		:stream		stream
		:eval-when	eval-when))
  (values))


(auto-doc-format defpackage
  "(defpackage <name> {option}*)"

  (format-distinguish
	:name		(second form)
	:form-type	"package"
	:stream		stream
	:eval-when	eval-when)
  (format stream "~@[ uses: ~a~]" (auto-doc-find (cddr form) :use))
  (format stream "~@[~%  nicknames: ~a~]"
	  (auto-doc-find (cddr form) :nicknames))
  (values))


(auto-doc-format defparameter
  "(defparameter <name> <initial-value> [<doc-string>])"

  (format-define-variable
	:name		(second form)
	:form-type	"parameter"
	:value		(third form)
	:documentation	(fourth form)
	:stream		stream
	:eval-when	eval-when))


(auto-doc-format defsetf
  "(defsetf <access-fn> ...)"

  (format-distinguish
	:name		(second form)
	:form-type	"setf method"
	:documentation	(etypecase (third form)
			  (symbol	(fourth form))
			  (list		(butlast (cddddr form))))
	:stream		stream
	:eval-when	eval-when))


(auto-doc-format defstruct
  "(defstruct <name-and-options> [<doc-string>] {slot-description}*)"

  (format-distinguish
	:name		(etypecase (second form)
			  (symbol	(second form))
			  (list		(first (second form))))
	:form-type	"structure"
	:documentation	(and (stringp (third form)) (third form))
	:stream		stream
	:eval-when	eval-when))


(auto-doc-format deftype
  "(deftype <name> <lambda-list> [<decls>|<doc-string>] {form}*)"

  (format-distinguish
	:name		(second form)
	:form-type	"type"
	:documentation	(find-if #'stringp (cdddr form))
	:stream		stream
	:eval-when	eval-when))


(auto-doc-format defun
  "(defun <name> <lambda-list> [<decls>|<doc-string>] {form}*)"

  (format-define-function
	:name		(second form)
	:form-type	"function"
	:lambda-list	(third form)
	:decls		(form-declarations form)
	:documentation	(find-if #'stringp (butlast (cdddr form)))
	:stream		stream
	:eval-when	eval-when))


(auto-doc-format defvar
  "(defvar <name> [<initial-value> [<doc-string>]])"

  (format-define-variable
	:name		(second form)
	:form-type	"variable"
	:value		(third form)
	:documentation	(fourth form)
	:stream		stream
	:eval-when	eval-when))


(auto-doc-format eval-when
  "(eval-when ({situation}*) {form}*)"

  (let ((new-eval-when (append eval-when (second form))))
       (aver (cons new-eval-when))

       (dolist (body-form (cddr form))
	       (aver (cons body-form))
	       (auto-doc-form body-form stream verbose new-eval-when map))))


(auto-doc-format export
  "(export <symbols> &optional <package>)"

  (format-distinguish
	:name		(second form)
	:form-type	"exported symbols"
	:stream		stream
	:eval-when	eval-when))


(auto-doc-format (flet labels let let*)
  "(<let-op> ({var|(var value)}*) {decl}* {form}*)"

  (dolist (let-form (cddr form))
	  (aver (cons let-form))
	  (auto-doc-form let-form stream verbose eval-when map))
  (values))


(auto-doc-format (if unless when)
  "(<if-op> <test> {form}*)"

  (dolist (if-form (cddr form))
	  (aver (cons if-form))
	  (auto-doc-form if-form stream verbose eval-when map))
  (values))


(auto-doc-format in-package
  "(in-package <name>)"
  ;; null function body means that the form is ignored
)


(auto-doc-format load
  "(load <file>)"
  ;; null function body means that the form is ignored
)


(auto-doc-format (setf setq setq-default)
  "(<set-op> {place newvalue}*)"

  (format stream
	  (format nil "~~%~~{~~%set ~~a to ~~:s ~@[ (eval-when ~a)~]~~}"
		  eval-when)
	  (rest form))
  (values))


(auto-doc-format set-dispatch-macro-character
  "(set-dispatch-macro-character <disp-char> <sub-char> <func> [<readtable>])"

  (format-new-entry stream	(intern (format nil "~a~a"
						(second form) (third form))))
  (format-form-type stream	"dispatch macro character" eval-when)
  (format stream "~%  function: ~a" (fourth form))
  (format stream "~@[~%  readtable: ~a~]" (fifth form))
)


(auto-doc-format set-macro-character
  "(set-macro-character <char> <function> [<non-terminating-p> [<readtable>]])"
  ;; null function body means that the form is ignored
)
