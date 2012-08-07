;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(apsb-utilities);-*-
;;;
;;;	temporary (and bogus) implementations
;;;

(in-package apsb-utilities)

#!(export '(collect scan-file))


;; declaration
(declaim (function collect (list) list))
(declaim (function scan-file (file-spec &optional reader) list))


;; exported function definitions
(defun collect (list) list)

(defun scan-file (file-spec &optional (reader #'read))
	"Dick Waters' scan-file, but this returns a list instead of a series."
	(aver (file-spec file-spec)
	      (function-spec reader))
	(with-open-file (stream file-spec)
		(aver (stream stream))
		(let ((eof-value (gentemp))
		      (file-values nil)
		      (value))
		     (aver (moniker eof-value)
			   (list file-values))
		     (loop (when (eql eof-value
				      (setq value
					    (funcall
						reader stream nil eof-value)))
				 (return (nreverse file-values)))
			   (push value file-values)))))
