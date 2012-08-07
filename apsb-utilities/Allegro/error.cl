;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(apsb-utilities);-*-
;;;
;;;	error definitions
;;;

(in-package apsb-utilities)

#!(export '(errno perror))


(eval '(defmacro get-sys_errlist ()
		(error "THIS FILE MUST BE COMPILED.")))


(eval-when (:compile-toplevel)
	(multiple-value-bind
		(shell-status shell-output)
		(shell
		  (namestring (make-pathname
				:defaults   (translate-logical-pathname
					      "Draco:apsb-utilities;Allegro;")
				:name	    "sys_errlist"
				:type	    "csh"))
		  :output nil)
		(declare (ignore shell-status))
		(aver (list shell-output))

		(eval `(defmacro get-sys_errlist ()
				,(apply #'vector shell-output)))
	) ; multiple-value-bind
) ; eval-when


(defconstant sys_errlist
	(get-sys_errlist)
	"vector of strings describing errno codes"
)


;; declarations
(declaim (simple-vector sys_errlist))

(declaim (function perror (&key prefix stream) (or null string)))

#+:Allegro (ff:defforeign
		'get-errno
		:entry-point (ff:convert-to-lang "get_errno")
		:return-type :integer)


;; exported definitions
(deftype errno () `(integer 1 ,(length sys_errlist)))


(defun perror (&key (prefix nil) (stream nil))
	"see the Unix man page for perror"
	(aver ((optional string) prefix)
	      ((optional stream) stream))
	(format stream "~@[~a: ~]~a~%"
		prefix (svref sys_errlist (1- (get-errno)))))
