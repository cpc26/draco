;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	identify the calibration frames that should be used to reduce the
;;;	remaining calibration/science data.  create averaged calibration
;;;	frames as needed and reduce data accordingly.  implemented via
;;;	the IRAF (NOAO) procedure ccdproc.
;;;

(in-package draco)


;; constants (duh!)
;; format strings
(defconstant	*no-data-format*
	"no files of type ~A for ~A."
	"arguments: file-type, file name (string)"
)
(defconstant	*NOAO-average-image-list-format*
	"echo ~A >> ~A"
	"string arguments: image name, image-list name"
)
(defconstant	*NOAO-ccdproc-comment-format*
	"remove ~a signature"
	"string argument: calibration file-type name"
)
(defconstant	*NOAO-ccdproc-format*
	"ccdproc ~a ~a=\"~a\" ccdtype=\"\" ~a"
	"string arguments: data image, calib. type, calib. image, ccdproc args"
)
(defconstant	*NOAO-combine-comment-format*
	"create averaged ~a files"
	"string argument: calibration file-type name"
)
(defconstant	*NOAO-combine-format*
	"~a @~a output=\"~a\" ccdtype=\"\" clobber=yes process=no"
	"string arguments: procedure name, input image-list name, output image"
)


;; special variable
(defparameter	*default-calibration-delta*
	(* 12 60 60)	; 12-hour delta = one-day interval
	"half the width of the calibration data time interval in seconds"
)


;; declarations
(declaim (string *no-data-format* *NOAO-average-image-list-format*
		 *NOAO-ccdproc-comment-format* *NOAO-ccdproc-format*
		 *NOAO-combine-comment-format* *NOAO-combine-format*))
(declaim (integer *default-calibration-delta*))

(declaim (inline NOAO-flatten NOAO-remove-bias NOAO-remove-dark))


;; class/type definitions
(defclass average-record (internal-entity)
	(
	 (name
		:type		moniker
		:documentation	"encoding of universal time interval"
		:initarg	:name
		:reader		name)
	 (file-type
		:type		file-type
		:documentation	"file-type of averaged image"
		:initarg	:file-type
		:reader		file-type)
	 (average
		:type		file-spec
		:documentation	"pathname of averaged image"
		:initform	(namestring
				  (make-pathname
					:defaults	(script-tempnam)
					:type		"imh"))
		:reader		average)
	 (image-list
		:type		file-spec
		:documentation	"pathname of averaged image image-list"
		:accessor	image-list)
	 (images
		:type		cons
		:documentation	"images used to create average"
		:initarg	:images
		:reader		images)
	)
	(:documentation	"maps averaged images to their constituent images")
	(:metaclass Draco-class)
)

(defmethod print-object ((average-record average-record) (stream stream))
	(print-unreadable-object (average-record stream :type t :identity t)
		(format stream "~a (~a)"
			(average average-record) (name average-record))))


(defclass calibration-record (internal-entity)
	(
	 (image
		:type		file-spec
		:documentation	"pathname of image to calibrate"
		:initarg	:image
		:reader		image)
	 (average-record
		:type		average-record
		:documentation	"calibration data for image"
		:initarg	:average-record
		:reader		average-record)
	)
	(:documentation	"maps images to relevant calibration data")
	(:metaclass Draco-class)
)

(defmethod print-object ((calib-record calibration-record) (stream stream))
	(print-unreadable-object (calib-record stream :type t :identity t)
		(format stream "~a (~a)"
			(image calib-record)
			(name (average-record calib-record)))))


;; constructor
(defmethod make-calibration-record (image (average-record average-record))
	"creates and returns a new calibration-record"
	(aver (file-spec image))
	(make-instance 'calibration-record
		:image		image
		:average-record	average-record))


;; internal methods
(defmethod NOAO-ccdproc
	((stream stream) (calibration-record calibration-record))
	"
	write commands for reducing data as specified
	by calibration-record to stream"

	(with-accessors ((image image)
			 (average-record average-record)) calibration-record
		(add-package-command stream
			(format nil *NOAO-ccdproc-format*
				image
				(NOAO-ccdproc-type calibration-record)
				(average average-record)
				(NOAO-ccdproc-args
					(file-type average-record))))))


(defmethod NOAO-ccdproc-args ((file-type file-type))
	"
	return appropriate noao.imred.ccdred ccdproc procedure
	arguments or nil if file-type isn't recognized"
	(case (name file-type)
	  (OIF-bias	"darkcor=no flatcor=no")
	  (OIF-dark	"zerocor=no flatcor=no")
	  (OIF-flat	"zerocor=no darkcor=no")
	))


(defmethod NOAO-ccdproc-type ((calibration-record calibration-record))
	"return ccdproc parameter appropriate for calibration-record"
	(ecase (name (file-type (average-record calibration-record)))
	  (OIF-bias	"zero")
	  (OIF-dark	"dark")
	  (OIF-flat	"flat")
	))


(defmethod NOAO-combine
	((stream stream) (average-record average-record))
	"
	write commands for creating an averaged calibration
	file as specified by average-record to stream"

	(with-accessors ((file-type file-type)
			 (average average)
			 (image-list image-list)
			 (images images)) average-record
		(let ((image-names (mapcar #'file-name images)))
		     (aver (cons image-names))
		     (setf image-list
			   (make-pathname
				:defaults	average
				:type		"lst"))
		     (dolist (image-name image-names)
			     (aver (file-spec image-name))
			     (add-command stream
				(format nil *NOAO-average-image-list-format*
					image-name image-list)))
		     (add-package-command stream
			(format nil *NOAO-combine-format*
				(NOAO-combine-procedure file-type)
				image-list average)))))


(defmethod NOAO-combine-procedure ((file-type file-type))
	"
	return noao.imred.ccdred combine procedure appropriate
	for file-type, or nil if file-type isn't recognized"
	(case (name file-type)
	  (OIF-bias	"zerocombine")
	  (OIF-dark	"darkcombine")
	  (OIF-flat	"flatcombine")
	))


(flet
  ((get-interval (sorted-list start end &key (key #'identity))
	"
	return elements of sorted-list between start and end inclusive;
	also returns first and last element values"
	(aver (list sorted-list)
	      (integer start end)
	      (function key))
	(let ((interval nil)
	      (inside-interval-p nil)
	      (value 0)
	      (first-value 0)
	      (last-value 0))
	     (aver (list interval)
		   (boolean inside-interval-p)
		   (integer value first-value last-value))
	     (dolist (element sorted-list)
		     (aver (t element))
		     (setq value (funcall key element))
		     (if inside-interval-p
			 (then (when (> value end) (return))
			       (push element interval)
			       (setq last-value value))
			 (else (when (>= value start)
				     (setq inside-interval-p	t
					   first-value		value)))))
	     (values interval first-value last-value))))

  (declare (function get-interval
		     (list integer integer &key key) list integer integer))

  (defmethod NOAO-calibrate ((stream stream)
			     directory
			     (calibration-file-type file-type)
			     &key (delta *default-calibration-delta*)
				  (earlier-only nil))
	"remove bias signal from calibration frames and science images"
	(aver (file-spec directory)
	      (file-type calibration-file-type)
	      (integer delta)
	      (boolean earlier-only))

	(setf	(instances 'average-record)	nil
		(instances 'calibration-record)	nil)

	(with-accessors ((name name)
			 (file-data file-data)) calibration-file-type
	   (let ((calib-files (sort (copy-seq (file-data calibration-file-type))
				   #'< :key #'ut-obs))
		 (interval nil)				; initial
		 (interval-start 0)			; values
		 (interval-end most-positive-fixnum))	; not used
		(aver (list calib-files interval)
		      (integer interval-start interval-end))

		(dolist (file-datum
			  (apply
			    #'nconc
			    (mapcar
			      #'copy-seq
			      (mapcar
				#'file-data
				(cdr (member calibration-file-type
					     (list (get-instance 'file-type
							:name	'OIF-bias)
						   (get-instance 'file-type
							:name	'OIF-dark)
						   (get-instance 'file-type
							:name	'OIF-flat)
						   (get-instance 'file-type
							:name	'OIF-comp)
						   (get-instance 'file-type
							:name	'OIF-object)
					     )))))))
			(aver (file-datum file-datum))

			(with-accessors ((file-name file-name)
					 (ut-obs ut-obs)) file-datum
			  (setq interval-start	(- ut-obs delta)
				interval-end	(+ ut-obs delta))
			  (when earlier-only
				(decf interval-start delta)
				(decf interval-end delta))
			  (multiple-value-setq
				(interval interval-start interval-end)
				(get-interval
					calib-files interval-start interval-end
					:key #'ut-obs))
			  (if interval
			      (then (make-calibration-record file-name
					(get-instance 'average-record
					  :name	(intern
						  (format nil "~a-~a"
							  interval-start
							  interval-end))
					  :file-type calibration-file-type
					  :images interval)))
			      (else (add-comment stream
					(format nil *no-data-format*
						name file-name))
				    (draco-inform *no-data-format*
					name file-name))))))

	   (when (instances 'average-record)
		 (add-comment stream
			(format nil *NOAO-combine-comment-format* name)))
	   (dolist (average-record (instances 'average-record))
		   (aver (average-record average-record))
		   (NOAO-combine stream average-record))

	   (when (instances 'calibration-record)
		 (add-comment stream
			(format nil *NOAO-ccdproc-comment-format* name)))
	   (dolist (calibration-record (instances 'calibration-record))
		   (aver (calibration-record calibration-record))
		   (NOAO-ccdproc stream calibration-record)))
	
	(nconc (mapcar #'image-list (instances 'average-record))
	       (mapcar #'average (instances 'average-record))))
); flet


;; method definitions
(defmethod NOAO-flatten ((stream stream) directory)
	"flatten a directory's science images via IRAF (NOAO) ccdproc"
	(aver (file-spec directory))
	(NOAO-calibrate
		stream directory (get-instance 'file-type :name 'OIF-flat)))


(defmethod NOAO-remove-bias ((stream stream) directory)
	"remove bias signal from a directory's images via IRAF (NOAO) ccdproc"
	(aver (file-spec directory))
	(NOAO-calibrate
		stream directory (get-instance 'file-type :name 'OIF-bias)))


(defmethod NOAO-remove-dark ((stream stream) directory)
	"remove dark signal from a directory's images via IRAF (NOAO) ccdproc"
	(aver (file-spec directory))
	(NOAO-calibrate
		stream directory (get-instance 'file-type :name 'OIF-dark)))
