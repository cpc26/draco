;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	search a directory for GEIS images that can be meaningfully combined
;;;

(in-package draco)


;; constant (duh!)
(defconstant	*findx-keywords*
	'(FILETYPE IMAGETYP INSTRUME FILTNAM1
	  FILTNAM2 RA_TARG  DEC_TARG DATATYPE)
	"keywords used to determine if two exposures may be combined"
)


;; declaration
(declaim (cons *findx-keywords*))

(declaim (function find-exposures (file-spec) list))


;; local function definition
(flet
  ((make-description (file-datum)
	"return string describing file-datum (in this context)"
	(aver (file-datum file-datum))
	(let ((description nil))
	     (aver (list description))
	     (dolist (keyword *findx-keywords*)
		     (aver (moniker keyword))
		     (push (assoc keyword (KV-pairs file-datum)) description))
	     (apply #'concatenate 'string description))))


  ;; local function declaration
  (declare (function make-description (file-datum) string))


  ;; internal function definition
  (defun find-exposures (directory)
	"find GEIS images that can be meaningfully combined"
	(aver (file-spec directory))

	(setf (tempnam-output :tag 'findx) nil)

	(let ((fds nil))
	     (aver (list fds))
	     (dolist (file-datum (file-data (get-instance 'file-type
					      :name 'GEIS-PODPS-calib-image)))
		     (aver (file-datum file-datum))
		     (push (cons (make-description file-datum) file-datum)
			   fds))
	     (do* ((fds (stable-sort (nreverse fds) #'string> :key #'car)
			(delete description fds :key #'car))
		   (description (caar fds) (caar fds)))

		  ((null fds))
		  (aver (list fds)
			((optional string) description))

		  (with-open-file (stream
				   (tempnam :directory	directory
					    :prefix	"findx"
					    :tag	'findx)
				   :direction :output)
			(aver (stream stream))
			(dolist (fd fds)
				(aver (cons fd))
				(unless (string= description (car fd))
					(return))
				(write-line (file-name (cdr fd)) stream)))))

	(tempnam-output :tag 'findx))
) ; flet
