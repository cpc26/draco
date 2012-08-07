;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(apsb-utilities);-*-
;;;
;;;	date utility
;;;

(in-package apsb-utilities)

#!(intern "DATE")
#!(defpackage user
	(:shadowing-import-from "APSB-UTILITIES"
		"DATE"
	)
)
#!(export '(date *date-default-map* day-string define-date-directive
	    month-string))


;; internal class
(defclass format-args ()
	(
	 (format-string
		:type		string
		:documentation	"a format string"
		:initform	""
		:initarg	:format-string
		:accessor	format-string)
	 (args
		:type		list
		:documentation	"arguments needed by FORMAT-STRING"
		:initform	nil
		:initarg	:args
		:accessor	args)
	)
	(:documentation "bundles a format string with the arguments it needs")
)

(defmethod print-object ((format-args format-args) (stream stream))
	(print-unreadable-object (format-args stream :type t :identity t)
		(princ (list (format-string format-args) (args format-args))
		       stream)))


;; constants (duh!)
(defconstant	*day-strings*
	(vector "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
	"map [0-6] to appropriate strings"
)
(defconstant	*month-strings*
	(vector ""
		"Jan" "Feb" "Mar" "Apr" "May" "Jun"
		"Jul" "Aug" "Sep" "Oct" "Nov" "Dec"
	)
	"map [1-12] to appropriate strings"
)


;; special variable
(defvar	*date-default-map*
	nil
	"
	alist mapping each apsb-utilities:date format directive to the
	corresponding format directive and required argument(s); new date
	directives are added using define-date-directive."
)


;; declarations
(declaim (type (vector string 7) *day-strings*))
(declaim (type (vector string 13) *month-strings*))
(declaim (list *date-default-map*))

(declaim (function date (&key :time :format) string))
(declaim (function day-string (fixnum) string))
(declaim (function make-format-args
		   (&rest list &key format-string args) format-args))
(declaim (function month-string (fixnum) string))

(declaim (inline day-string month-string))			; speed kills?


;; local function definition
(flet
  ((date-parse-format (date-format map &optional (arg-list nil))
	"return the appropriate format-args structure"
	(aver (string date-format)
	      (list map arg-list))

	(do ((date-format-length (length date-format))
	     (date-format-pos 0)
	     (format-args (make-format-args)))

	    ((= date-format-pos date-format-length)
	    (progn (setf (args format-args)
			 (append arg-list (args format-args)))
		   format-args))
	    (aver (fixnum date-format-length date-format-pos)
		  (format-args format-args))

	    (if (char= #\~ (elt date-format date-format-pos))
		(then (date-parse-directive
			(subseq date-format
				date-format-pos (incf date-format-pos 2))
			format-args
			map))
		(else (setf (format-string format-args)
			    (concatenate 'string
				(format-string format-args)
				(subseq date-format
					date-format-pos
					(incf date-format-pos 1)))))))))


  ;; local function declaration
  (declare (function date-parse-format
		     (string list &optional list) format-args))


  ;; internal class-specific method
  (defmethod date-parse-directive (directive (format-args format-args) map)
	"
	translates (date format) directive into an ordinary
	format directive modifying format-args accordingly."
	(aver (string directive)
	      (list map))

	(let ((directive-format-args (make-format-args
					:format-string directive))
	      (table-entry (assoc (elt directive 1) map)))
	     (aver (format-args directive-format-args)
		   (list table-entry))

	     (when table-entry
		   (setq directive-format-args
			 (date-parse-format
				(cadr table-entry) map (cddr table-entry))))

	     (setf (format-string format-args)
		   (concatenate 'string
			(format-string format-args)
			(format-string directive-format-args)))

	     (setf (args format-args)
		   (append (args format-args) (args directive-format-args))))
	(values))


  ;; exported function definition
  (defun date (&key ((:time universal-time)	(get-universal-time))
		    ((:format date-format)	"~d-~h-~y, ~T")
		    (map			*date-default-map*))
	"
	formats universal-time using the format string date-format;
	apsb-utilities:date format strings may contain directives
	that shadow the usual format directives, e.g. ~T.
	built-in directives include:

			~D	date as mm/dd/yy
			~H	hour - 00 to 23
			~M	minute - 00 to 59
			~S	second - 00 to 59
			~T	time as HH:MM:SS
			~a	abbreviated weekday - Mon to Sun
			~d	day of month - 01 to 31
			~h	abbreviated month - Jan to Dec
			~m	month of year - 01 to 12
			~t	insert a standard, 8-space tab
			~w	day of week - Monday = 0
			~y	year - e.g. 1992

	(this is modeled after /bin/date.)  the caller may specify
	new directives by modifying *date-default-map* or by creating
	his own map.

	for more information, see (describe '*date-default-map*),
	(describe 'define-date-directive), or (apsb-utilities:test date)."
	(aver (integer universal-time)
	      (string date-format)
	      (list map))

	(multiple-value-bind
	  (second minute hour date month year day-of-week)
	  (decode-universal-time universal-time)
	  (aver (fixnum second minute hour date month year))

	  (let ((format-args (date-parse-format date-format map)))
	       (aver (format-args format-args))

	       (apply #'format
		      `(nil ,(format-string format-args)
			,@(mapcar #'eval
			    (sublis
				(pairlis '("SECOND" "MINUTE" "HOUR" "DATE"
					   "MONTH" "YEAR" "DAY-OF-WEEK")
					 (list second minute hour date
					       month year day-of-week))
				(args format-args)
				:test #'string=
				:key #'(lambda (x)
					 (aver (t x))
					 (and (symbolp x)
					      (symbol-name x))))))))))


) ; flet


;; exported function definition
(defun day-string (day-of-week)
	"convert fixnum day-of-week to string"
	(aver (fixnum day-of-week))
	(aref *day-strings* day-of-week))


;; internal function definition
(defun make-format-args (&rest args)
	"format-args constructor"
	(aver (list args))
	(apply #'make-instance 'format-args args))


;; exported function definition
(defun month-string (month)
	"convert fixnum month to string"
	(aver (fixnum month))
	(aref *month-strings* month))


;; exported macro definition
(defmacro define-date-directive
	  (&key (directive		(error *missing-keyword-format*
					  "define-date-directive" :directive))
		(format			(error *missing-keyword-format*
					  "define-date-directive" :format))
		(args			nil)
		((:map map-name)	'*date-default-map*))
	"
	add date directive to map.  ~directive translates to the string
	specified by format and requires the arguments specified by args.
	the arguments may be built from the following built-in functions
	and fixnum variables:

	  functions: day-string month-string
	  variables: second minute hour date month year day-of-week

	additional functions may be defined, but the variable list is
	fixed.  BEWARE of ambiguous directives, e.g. ~D, because date
	directive translation is recursive.  if the ambiguous directive
	is to be consumed by common-lisp:format, try to prefix it with
	something, e.g. ~4D."
	(aver (standard-char directive)
	      (string format)
	      (list args)
	      (symbol map-name))

	`#!(progn (setq ,map-name
			(acons ',directive (cons ',format ',args) ,map-name))
		  ',map-name))


(auto-doc-format define-date-directive
	"(define-date-directive
		:directive <char> :format <string> [:args <list>])"
	;; null function body means that the form is ignored
)
