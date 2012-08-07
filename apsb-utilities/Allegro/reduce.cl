;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(common-lisp);-*-

(in-package common-lisp)


#+:Allegro
(excl:without-package-locks
	(setf (fdefinition 'common-lisp:reduce)
	      (fdefinition 'Norvig:reduce))
)

#|
;; this is good code, but the package system makes it hard to use
(define-compiler-macro REDUCE (&whole form function sequence
			       &key from-end start end initial-value
				    (key nil key-p))
  (declare (ignore function sequence from-end start end initial-value key))
  "Create a compiler macro bypassing apsb-utilities:reduce"
  (cons (if key-p 'Norvig:reduce 'common-lisp:reduce)
	(cdr form)))
|#
