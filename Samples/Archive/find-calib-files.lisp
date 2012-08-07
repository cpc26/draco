;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	search a directory for calibration images to calibrate
;;;	a given object image
;;;

(in-package draco)

#!(export '(find-calibration-files))


;; declaration
(declaim (function find-calibration-files
		   (file-datum file-type &key delta earlier-only)))


(let ((findx-file-types nil)
      (user-file-types nil))
     (aver (list findx-file-types user-file-types))

     ;; local function definitions
     (labels
	((find-calibration-files-1 (directory)
		"find-calibration-files workhorse"
		(aver (file-spec directory))
		(setf (tempnam-output :tag 'findx) nil)
		(with-open-stream
		  (inventory (make-string-output-stream))
		  (aver (string-stream inventory))
		  (make-inventory
			:directory	directory
			:inventory	inventory
			:entries-only	t)
		  (with-input-from-string
		    (inventory-stream (get-output-stream-string inventory))
		    (aver (string-stream inventory-stream))
		    (dolist (pathnames (find-calibration-files-2 inventory-stream))
			    (aver (cons pathnames))
			    (with-open-file
				(stream
				 (tempnam :directory	directory
					  :prefix	"findx"
					  :tag		'findx)
				 :direction :output)
				(aver (stream stream))
				(dolist (pathname pathnames)
					(aver (pathname pathname))
					(write-line
					  (namestring pathname) stream))))))
		(tempnam-output :tag 'findx))


	 (find-calibration-files-2 (inventory-stream)
		"convert make-inventory output to a list of lists of pathnames"
		(aver (string-stream inventory-stream))
		(let ((inventory-line nil)
		      (pathnames nil)		; list of combinable images
		      (pathnames-list nil))	; list of pathnames lists
		     (aver ((optional string) inventory-line)
			   (list pathnames pathnames-list))
		     (loop
			(setq inventory-line
			      (read-line inventory-stream nil nil))
			(cond ((null inventory-line)		; eof test
			       (when pathnames
				     (push pathnames pathnames-list))
			       (return))
			      ((zerop (length inventory-line))
			       (when pathnames
				     (push pathnames pathnames-list))
			       (setq pathnames nil))
			      ((not (eql #\space (schar inventory-line 0)))
			       (push (pathname
					(subseq inventory-line
					  0 (position #\space inventory-line)))
				     pathnames))))
		     pathnames-list))


	 (initialize ()
		"establish local file-types"
		(setf (instances 'file-type) nil)
		(define-internal-file-type
			:name		findx-GEIS-PODPS-calib-image
			:documentation	"GEIS (calibrated) image"
			:recognizer	#'GEIS-PODPS-calib-image-p
			:reporter
				(define-KV-reporter
				  :name			findx-GEIS-reporter
				  :documentation
				      "Unix lex-function :c-sorted KV reporter"
				  :lex-function		(make-pathname
							  :defaults *tools-dir*
							  :name	    "GEIS-lex")
				  :reporter-type	:c-sorted
			)
			:reporter-type	:c-sorted
		)
		(define-report-keywords
			:reporter	findx-GEIS-reporter
			:keywords	(FILETYPE IMAGETYP INSTRUME FILTNAM1
					 FILTNAM2 RA_TARG  DEC_TARG DATATYPE)
		)
		(setq findx-file-types (instances 'file-type))
		(values)))


	;; local function declarations
	(declare (function find-calibration-files-1 (file-spec symbol) list)
		 (function find-calibration-files-2 (stream) list)
		 (function initialize ()))


	;; internal function definition
	(defun find-calibration-files
	       (object-image calibration-file-type
		&key (delta *default-calibration-interval-width*)
		     (earlier-only nil))
		"associate object-image with its calibration images"
		(aver (file-datum object-image)
		      (file-type calibration-file-type)
		      (integer delta)
		      (boolean earlier-only))

		; do we take inventory or assume that we already have?

		(setq user-file-types (instances 'file-type))
		(unwind-protect
			(progn	(or (setf (instances 'file-type)
					  findx-file-types)
				    (initialize))
				(find-calibration-files-1 directory))
			(setf (instances 'file-type) user-file-types))
		(values))
     ) ; labels
) ; let
