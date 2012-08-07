;;; startup message, per page T-4 of "Delivering Applications Written
;;; in Allegro CL
;;; Glenn Miller 12/16/93


;;; ADD THIS TO SYSTEM.LISP

(in-package draco)

(defvar *draco-startup-message*
    (format nil 
	    ";;; Draco - Data Reduction Expert Assistant - V~a - STScI~2%"
	    *version*)
  "identification string fro draco in startup message")

#+:Allegro (push '(:app . nil) excl:*print-startup-message*)

#+:Allegro (defmethod excl:print-startup-info ((type (eql :app))
					       &optional brief)
	     (declare (ignore brief))
	     (format *terminal-io* *draco-startup-message*))

