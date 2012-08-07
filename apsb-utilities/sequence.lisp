;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(apsb-utilities);-*-
;;;
;;;	sequence utilities
;;;

(in-package apsb-utilities)

#!(export '(find-every has-duplicates set-equal typed-vector))


;; constant (duh!)
;; format string
(defconstant *typed-vector-error-format*
	"~&apsb-utilities:typed-vector: element type error"
)


;; declarations
(declaim (string *typed-vector-error-format*))

(declaim (function find-every (function-spec sequence &key start end key) list))
(declaim (function has-duplicates (sequence &key key test) boolean))
(declaim (function set-equal (sequence sequence &key key test) boolean))
(declaim (function typed-vector
		   ((or cons symbol) t &key inhibit-checking) simple-vector))

(declaim (inline find-every set-equal))


;; exported function definition
(defun find-every (predicate sequence
		   &key (start 0) (end nil) (key #'identity))
	"returns every item in sequence that satisfies predicate"
	(aver (function-spec predicate)
	      (sequence sequence)
	      (fixnum start)
	      ((optional fixnum) end)
	      (function-spec key))
	(remove-if (complement predicate) (subseq sequence start end)
		:key key))


;; local function definition
(flet
  ((has-duplicates1 (list key test)
	"returns T if list has duplicates; returns nil otherwise"
	(aver (list list)
	      (function key test))
	(loop	(when (null list)
		      (return nil))
		(when (find (pop list) list :key key :test test)
		      (return t)))))	; the duplicate element might be nil!

  ;; local function declaration
  (declare (function has-duplicates1 (list function function) boolean))

  ;; exported function definitions
  (defun has-duplicates (sequence &key (key #'identity) (test #'eql))
	"returns T if sequence has duplicates; returns nil otherwise"
	(aver (sequence sequence)
	      (function key test))
	(has-duplicates1 (coerce sequence 'list) key test))
) ; flet


(defun set-equal (s1 s2 &key (key #'identity) (test #'eql))
	"returns T if sequence s1 equals sequence s2; returns nil otherwise"
	(aver (sequence s1 s2)
	      (function key test))
	(not (set-exclusive-or (coerce s1 'list) (coerce s2 'list)
		:key key  :test test)))


(defun typed-vector (element-type elements &key (inhibit-checking nil))
	"
	returns a simple-vector of the specified type.  if elements is
	a sequence that is not a string, it becomes the :initial-contents
	list; otherwise, :initial-contents is (list elements)"
	(aver ((or cons moniker standard-class) element-type)
	      (t elements)
	      (boolean inhibit-checking))

	(typecase elements
		(string		(setq elements (list elements)))
		(sequence	)
		(t		(setq elements (list elements)))
	)

	(unless inhibit-checking
		(unless (every #'(lambda (element)
					 (typep element element-type))
			       elements)
			(error *typed-vector-error-format*)))

	(make-array (list (length elements))
		:element-type		element-type
		:initial-contents	elements))


(define-test-function find-every
  :test #'equalp
  ((find-every #'characterp "kali" :key #'char-upcase)	"kali")
  ((find-every #'characterp "kali" :start 1)		"ali")
  ((find-every #'characterp "kali" :end 1)		"k")
)

(define-test-function has-duplicates
  ((has-duplicates "kali" :key #'char-upcase)	nil)
  ((has-duplicates "Bob" :test #'char-equal)	t)
  ((has-duplicates #())				nil)
  ((has-duplicates '(1))			nil)
)

(define-test-function set-equal
  ((set-equal "Bali" "bail" :test #'char-equal)	t)
  ((set-equal "Bali" "bail" :key #'char-upcase)	t)
  ((set-equal "Bali" "ball" :test #'equalp)	nil)
)

(define-test-function typed-vector
  :test #'equalp
  ((typed-vector 'character '(#\f #\o #\o))	"foo")
  ((typed-vector 't         '(#\f #\o #\o))	#(#\f #\o #\o))
)
