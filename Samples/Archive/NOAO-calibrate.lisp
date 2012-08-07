;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	identify the calibration frames that should be used to reduce the
;;;	remaining calibration/science data.  create averaged calibration
;;;	frames as needed and reduce data accordingly.  implemented via
;;;	the IRAF (NOAO) procedure ccdproc.
;;;

(in-package draco)

#!(export '(NOAO-flatten NOAO-remove-bias NOAO-remove-dark))


;; constants (duh!)
;; format strings
(defconstant	*insufficient-data-format*
	"~&Draco:NOAO-calibrate: no files of type ~A for ~A."
	"arguments: file-type, file name (string)"
)
(defconstant	*NOAO-ccdproc-format*
	"ccdproc \"~a, ~a\""
	"string arguments: data/calibration image, calibration/data image"
)
(defconstant	*NOAO-combine-format*
	"combine ~a ~a ~a"
	"string arguments: input images, output image, combine parameters"
)


;; special variable
(defparameter	*default-calibration-delta*
	(* 5 60 60)	; 5 hours
	"half the width of the calibration data time interval in seconds"
)


;; declarations
(declaim (string *insufficient-data-format* *NOAO-ccdproc-format*
		 *NOAO-combine-format*))
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
		:initform	(tempnam :prefix "Draco")
		:reader		average)
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
			(average average-record) (interval average-record))))


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
			(interval (average-record calib-record)))))


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
		(format stream *NOAO-ccdproc-format*
			image (average average-record))))


(defmethod NOAO-combine
	((stream stream) (average-record average-record))
	"
	write commands for creating an averaged calibration
	file as specified by average-record to stream"

	(with-accessors ((file-type file-type)
			 (average average)
			 (images images)) average-record
		(format stream *NOAO-combine-format*
			images average (NOAO-combine-parameters file-type))))


(defmethod NOAO-combine-parameters ((file-type file-type))
	"
	return noao.imred.ccdred.combine parameters appropriate
	for file-type, or nil if file-type isn't recognized"
	(typecase (name file-type)
	  (OIF-bias	"process=no nlow=0 blank=0. reject=\"minmax\" scale=\"none\"")
	  (OIF-dark	"process=yes nlow=1 blank=0. reject=\"avsigclip\" scale=\"exposure\"")
	  (OIF-flat	"process=yes nlow=1 blank=1. reject=\"avsigclip\" scale=\"mode\"")
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

	(let ((calib-files (sort (copy-seq (instances calibration-file-type))
				 #'> :key #'ut-obs))
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
			(when (null interval)
			      (draco-warning *insufficient-data-format*
				calibration-file-type file-name))
			(make-calibration-record file-name
				(get-instance 'average-record
					:name	(format nil "~a-~a"
							interval-start
							interval-end)
					:file-type calibration-file-type
					:images	interval)))))

	(map nil #'NOAO-combine (instances 'average-record))
	(map nil #'NOAO-ccdproc (instances 'calibration-record))
	(values))
); flet


;; exported methods
(defmethod NOAO-flatten ((stream stream) directory
			 &key (delta *default-calibration-delta*)
			      (earlier-only nil))
	"flatten a directory's science images via IRAF (NOAO) ccdproc"
	(aver (file-spec directory)
	      (integer delta)
	      (boolean earlier-only))
	(NOAO-calibrate
		stream directory (get-instance 'file-type :name 'OIF-flat)
		:delta delta :earlier-only earlier-only))


(defmethod NOAO-remove-bias ((stream stream) directory
			     &key (delta *default-calibration-delta*)
				  (earlier-only nil))
	"remove bias signal from a directory's images via IRAF (NOAO) ccdproc"
	(aver (file-spec directory)
	      (integer delta)
	      (boolean earlier-only))
	(NOAO-calibrate
		stream directory (get-instance 'file-type :name 'OIF-bias)
		:delta delta :earlier-only earlier-only))


(defmethod NOAO-remove-dark ((stream stream) directory
			     &key (delta *default-calibration-delta*)
				  (earlier-only nil))
	"remove dark signal from a directory's images via IRAF (NOAO) ccdproc"
	(aver (file-spec directory)
	      (integer delta)
	      (boolean earlier-only))
	(NOAO-calibrate
		stream directory (get-instance 'file-type :name 'OIF-dark)
		:delta delta :earlier-only earlier-only))
