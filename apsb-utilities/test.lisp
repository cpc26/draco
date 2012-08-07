;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(apsb-utilities);-*-
;;;
;;;	test function generator
;;;
;;;	known bugs/limitations:	does not support (setf ...) functions
;;;

(in-package apsb-utilities)

#!(export '(*test-default-map* define-test-function-fn test))


;; constants (duh!)
;; format strings
(defconstant	*run-test-format*
	"~&(test ~A)~%"
	"arguments: test name"
)
(defconstant	*test-enabled-format*
	"~&(test ~A) enabled (~a)~%"
	"arguments: test name, map name"
)
(defconstant	*test-not-enabled-format*
	"~&(test ~A) not yet enabled (~a)~%"
	"arguments: test name, map name"
)
(defconstant	*testing-123-format*
	"~&testing ~A, 123 ..."
	"arguments: test name"
)


;; special variable
(defvar	*test-default-map*
	nil
	"alist mapping functions to their test functions"
)


;; declarations
(declaim (string *run-test-format* *test-enabled-format*
		 *test-not-enabled-format* *testing-123-format*))
(declaim (list *test-default-map*))

(declaim (function define-test-function-fn (moniker list)))
(declaim (function test-fn (list) boolean))


;; local function definitions
(labels
  (
   (missing-test (test map-name)
	"missing tests always fail"
	(aver (t test)
	      (symbol map-name))
	(format *diagnostic-stream* *test-not-enabled-format*
		(test-full-name test) map-name)
	(values))


   (test-full-name (test)
	"return test name with package prefix"
	(aver (symbol test))
	(concatenate 'string
		     (package-name (symbol-package test))
		     ":"
		     (symbol-name test))))


  ;; local function declarations
  (declare (function missing-test (t symbol))
	   (function test-full-name (symbol) string)

	   (inline missing-test test-full-name))


  ;; internal function definitions
  (defun define-test-function-fn (function-name body)
	"does everything for define-test-function"
	(aver (moniker function-name)
	      (list body))
	(let ((test-name (gentemp function-name))
	      (attributes (pairlis
			    '(key let map test)
			    (list #'identity nil '*test-default-map* #'equal)))
	      (forms body)
	      (tests nil)	; initialized later
	      (doc-string nil))
	     (aver (moniker test-name)
		   (cons attributes)
		   (list forms tests)
		   ((optional string) doc-string))
	     (loop (typecase (first forms)
			(symbol	(let ((first (intern
						(symbol-name (first forms)))))
				     (aver (symbol first))
				     (if (find first '(key let map test))
					 (then (rplacd (assoc first attributes)
						       (second forms))
					       (setq forms (cddr forms)))
					 (else (return)))))
			(string	(setq doc-string (pop forms))
				(return))
			(t	(return))
		   ))
	     (dolist (form forms)
		     (aver (cons form))
		     (push (list 'funcall (cdr (assoc 'test attributes))
				 (list 'mapcar (cdr (assoc 'key attributes))
				       (list 'multiple-value-list (car form)))
				 (list 'mapcar (cdr (assoc 'key attributes))
				       (list 'mapcar #'eval (cdr form))))
			   tests))
	     (eval `(declaim (function ,test-name () boolean)))
	     (setf (documentation test-name 'function)
		   (or doc-string
		       (format nil *testing-123-format*
				   (test-full-name function-name))))
	     (eval `(defun ,test-name ()
			   (let ,(cdr (assoc 'let attributes))
				(every
				  #'(lambda (form)
				      (aver (cons form))
				      (funcall ,(cdr (assoc 'test attributes))
					(mapcar ,(cdr (assoc 'key attributes))
					  (multiple-value-list
						(eval (car form))))
					(mapcar ,(cdr (assoc 'key attributes))
					  (mapcar #'eval (cdr form)))))
				  ',forms))))
	     (set (cdr (assoc 'map attributes))
		  (acons function-name test-name
			 (eval (cdr (assoc 'map attributes)))))
	     (when *load-verbose*
		   (format *diagnostic-stream* *test-enabled-format*
			   (test-full-name function-name)
			   (cdr (assoc 'map attributes)))))
	(values))


  (defun test-fn (body)
	"does everything for test"
	(aver (list body))
	(let ((tests body)
	      (map-name '*test-default-map*))
	     (aver (list tests)
		   (symbol map-name))
	     (when (and (symbolp (first body))
			(string= "MAP" (symbol-name (first body))))
		   (setq map-name	(second body)
			 tests		(cddr tests)))
	     (every #'(lambda (test)
			(aver (symbol test))
			(let ((pair (assoc (symbol-name test) (eval map-name)
					   :test #'string=)))
			     (aver (list pair))
			     (unless pair (missing-test test map-name))
			     (format *diagnostic-stream*
				     *run-test-format*
				     (test-full-name (car pair)))
			     (funcall (cdr pair))))
		    (or tests
			(remove-duplicates (mapcar #'car (eval map-name)))))))
) ; labels


;; exported macro definition
(defmacro test (&body body)
	"
	syntax:	(test [:map <map name>] [test]*)

	map-name defaults to '*test-default-map*.  if no tests are
	specified, all tests in the specified map are invoked."
	`#!(test-fn ',body))


(auto-doc-format define-test-function
	"
	(define-test-function <name>
		[:map <map-name>]
		[:key <key-name>]
		[:test <test-name>]
		[:let <bindings>]
		[documentation]
		[(<form> <values>)]*)"

	(let* ((test-name (second form))
	       (form (cddr form))
	       (map-name '*test-default-map*)
	       (key 'identity)
	       (test-fn nil)		; initialized later
	       (let nil))
	      (aver (moniker test-name)
		    (cons form)
		    (moniker map-name key)
		    (symbol test-fn)
		    (list let))

	      (loop (typecase (first form)
			(symbol	(let ((first-form-name (symbol-name
							 (first form))))
				     (aver (string first-form-name))

				     (cond ((string= "KEY" first-form-name)
					    (setq key	(cadr (second form))
						  form	(cddr form)))
					   ((string= "LET" first-form-name)
					    (setq let	(second form)
						  form	(cddr form)))
					   ((string= "MAP" first-form-name)
					    (setq map-name (second form)
						  form	(cddr form)))
					   ((string= "TEST" first-form-name)
					    (setq form	(cddr form)))
					   (t (return)))))

			(string	(pop form)	; documentation string
				(return))

			(t	(return))))

	      (setq test-fn
		    (cdr (assoc (symbol-name test-name) (eval map-name)
				:test #'string=)))

	      (format-new-entry stream test-fn)
	      (format-form-type stream
		(format nil "(apsb:test ~a) function" test-name)
		eval-when)
	      (format-documentation stream (documentation test-fn 'function))
	      (format stream "~@[~%  LOCAL BINDINGS: ~s~]" let)
	      (unless (eq map-name '*test-default-map*)
		      (format stream "~%  MAP: ~s" map-name))
	      (unless (eq key 'identity)
		      (format stream "~%  KEY: ~s" key))

	      (dolist (call-value form)
		      (aver (cons call-value))
		      (format stream "~%  ~s  --> ~{ ~s~}"
			      (first call-value) (cdr call-value))))
	(values)
)
