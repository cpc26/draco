;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	find an image's observation time
;;;

(in-package draco)


;; constant (duh!)
;; format string
(defconstant	*canonical-time-failure-format*
        "~&~A is not a legal time specification."
        "argument: string"
)


;; declaration
(declaim (string *canonical-time-failure-format*))


;; class/type definitions
(deftype second-minute-integer ()
        "legal minute/second values"
        '(mod 60))

(deftype hour-integer ()
        "legal hour values"
        '(mod 24))


;; local function definitions
(labels
  ((canonical-time (ut)
	"
	return second, minute, and hour corresponding to ut;
	addresses ctio bug that produces times like 14:46:60.0"
	(aver (string ut))
	(let ((ut (substitute-if #\space (complement #'digit-char-p) ut))
	      (index 0)
	      (second 0)
	      (minute 0)
	      (hour 0))
	     (aver (string ut)
		   (integer index second minute hour))
	     (multiple-value-setq (hour index)
				  (read-from-string ut nil 0))
	     (multiple-value-setq (minute index)
				  (read-from-string ut nil 0 :start index))
	     (multiple-value-setq (second index)
				  (read-from-string ut nil 0 :start index))
	     (unless (typep second 'second-minute-integer)
		     (incf minute (floor second 60))
		     (setf second (mod second 60)))
	     (unless (typep minute 'second-minute-integer)
		     (incf hour (floor minute 60))
		     (setf minute (mod minute 60)))
	     (unless (typep hour 'hour-integer)
		     (draco-error *canonical-time-failure-format* ut))
	     (values second minute hour)))

   (ctio-time (date ut)
	"return universal time extracted from date and ut strings"
	(aver (string date ut))
	(let ((date (substitute-if #\space (complement #'digit-char-p) date))
	      (index 0)
	      (month 0)
	      (day 0))
	     (aver (string date)
		   (integer index month day))
	     (multiple-value-bind
		(second minute hour)
		(canonical-time ut)
		(aver (integer second minute hour))
		(encode-universal-time
			second minute hour
			(multiple-value-setq
				(day index)
				(read-from-string date nil 0))
			(multiple-value-setq
				(month index)
				(read-from-string date nil 0 :start index))
			(read-from-string date nil 0 :start index))))))


  ;; local function declarations
  (declare (function canonical-time (string) integer integer integer)
	   (function ctio-time (string string) integer))


  ;; internal method definition
  (defmethod ut-obs ((file-datum file-datum))
	"create ut-obs KV-pair if necessary/possible"
	(with-accessors ((KV-pairs KV-pairs)) file-datum
		(let ((ut-obs (cdr (assoc 'ut-obs KV-pairs))))
		     (aver ((optional integer) ut-obs))
		     (unless ut-obs
			     (let ((date-obs (cdr (assoc 'date-obs KV-pairs)))
				   (ut (or (cdr (assoc 'ut KV-pairs))
					   (cdr (assoc 'time-obs KV-pairs)))))
				  (aver ((optional string) date-obs ut))
				  (setq ut-obs
					(if (and date-obs ut)
					    (then (ctio-time date-obs ut))
					    (else 0)))
				  (setf KV-pairs
					(acons 'ut-obs ut-obs KV-pairs))))
		     ut-obs)))
) ; labels
