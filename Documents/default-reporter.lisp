;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	Draco default report definitions
;;;

(in-package draco)


;; constant (duh!)
;; format string
(defconstant	*inventory-mystery-entry-format*
	"~&;;; ~A: ~31,0T unrecognized~%"
	"arguments: file name"
)


;; declarations
(declaim (string *inventory-mystery-entry-format*))

(declaim (function write-inventory-mystery-entry (string &optional stream)))


;; internal class-specific method definition
(defmethod write-default-report ((file-datum file-datum) data-directory
				 &optional (report-stream *standard-output*))
	"write a default report"
	(declare (ignore data-directory))
	(aver (stream report-stream))
	(print-readable-object file-datum report-stream)
	(values))


;; internal function definition
(defun write-inventory-mystery-entry
       (file-name &optional (output-stream *standard-output*))
	"write an inventory mystery entry"
	(aver (string file-name)
	      (stream output-stream))
	(format output-stream *inventory-mystery-entry-format* file-name)
	(values))
