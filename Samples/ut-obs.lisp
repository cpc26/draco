;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	find an image's observation time
;;;

(in-package draco)


;; class/type definitions
(deftype second-minute-integer ()
        "legal minute/second values"
        '(mod 60))

(deftype hour-integer ()
        "legal hour values"
        '(mod 24))


;; local function definitions
(flet
  ((ctio-time (date ut)
	"return universal time extracted from date and ut strings;
	addresses ctio bug that produces times like 14:46:60.0"
	(aver (string date ut))
	(let ((date (substitute-if #\space (complement #'digit-char-p) date))
	      (ut (substitute-if #\space (complement #'digit-char-p) ut))
	      (correction 0)
	      (index 0)
	      (second 0)
	      (minute 0)
	      (hour 0)
	      (month 0)
	      (day 0))
	     (aver (string date ut)
		   (integer correction index second minute hour month day))
	     (multiple-value-setq (hour index)
				  (read-from-string ut nil 0))
	     (multiple-value-setq (minute index)
				  (read-from-string ut nil 0 :start index))
	     (multiple-value-setq (second index)
				  (read-from-string ut nil 0 :start index))
	     (unless (typep second 'second-minute-integer)
		     (incf correction second)
		     (setq second 0))
	     (unless (typep minute 'second-minute-integer)
		     (incf correction (* 60 minute))
		     (setq minute 0))
	     (unless (typep hour 'hour-integer)
		     (incf correction (* 60 60 hour))
		     (setq hour 0))
	     (+ correction
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
  (declare (function ctio-time (string string) integer))


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
) ; flet
