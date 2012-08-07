;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(apsb-utilities);-*-
;;;
;;;	list utilities
;;;

(in-package apsb-utilities)

#!(export '(true-list))


;; declaration
(declaim (function true-list (list) list))


;; exported function definition
(defun true-list (list)
	"
	if list is dotted, return a true list containing the same
	\"elements\"; otherwise, #'true-list is just like #'identity."
	(aver (list list))
	(let ((last (last list)))
	     (aver (list last))
	     (if (cdr last)
		 (then (nconc (ldiff list last)
			      (list (car last) (cdr last))))
		 (else list))))


(define-test-function true-list
	:test #'equal
	((true-list	nil)
	 nil)
	((true-list	'(a))
	 '(a))
	((true-list	'(b c))
	 '(b c))
	((true-list	'(d . e))
	 '(d e))
	((true-list	'(f g . h))
	 '(f g h))
)
