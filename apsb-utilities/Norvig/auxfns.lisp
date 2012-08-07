;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
(in-package Norvig)

;;; Code from Paradigms of AI Programming
;;; Copyright (c) 1991 Peter Norvig

;;; File auxfns.lisp: Auxiliary functions used by all other programs
;;; Load these before running any other programs.

;;; This is a stripped-down version of auxfns.lisp, containing only those
;;; functions we want.

;;; This file requires the macros in auxmacs.lisp

;;; auxfns.lisp,v

;;; Revision 1.1  1992/05/10  19:54:04  mrose
;;; Initial revision
;;;

;;; History:
;;; Apr 12 1992 mrose, created, changed package from USER to NORVIG, renamed
;;;                    DEBUG to DBG-ON, and UNDEBUG to DBG-OFF, changed
;;;                    implementation, but not interface of dbg and dbg-indent.


;;;; Reduce - Support :key keyword for the REDUCE function

(declaim (inline reduce reduce*))

(defun reduce* (fn seq from-end start end key init init-p)
  (funcall (if (listp seq) #'reduce-list #'reduce-vect)
           fn seq from-end (or start 0) end key init init-p))

(defun REDUCE (function sequence &key from-end start end key
               (initial-value nil initial-value-p))
  (reduce* function sequence from-end start end
           key initial-value initial-value-p))

(defun reduce-vect (fn seq from-end start end key init init-p)
  (if (null end) (setf end (length seq)))
  (assert (<= 0 start end (length seq)) (start end)
          "Illegal subsequence of ~a --- :start ~d :end ~d"
          seq start end)
  (case (- end start)
    (1 (if init-p
           (funcall fn init (funcall-if key (aref seq start)))
           (funcall-if key (aref seq start))))
    (0 (if init-p init (funcall fn)))
    (t (if (not from-end)
           (let ((result
                   (if init-p
                       (funcall
                         fn init
                         (funcall-if key (aref seq start)))
                       (funcall
                         fn
                         (funcall-if key (aref seq start))
                         (funcall-if key (aref seq (+ start 1)))))))
             (loop for i from (+ start (if init-p 1 2))
                   to (- end 1)
                   do (setf result
                            (funcall
                              fn result
                              (funcall-if key (aref seq i)))))
             result)
           (let ((result
                   (if init-p
                       (funcall
                         fn
                         (funcall-if key (aref seq (- end 1)))
                         init)
                       (funcall
                         fn
                         (funcall-if key (aref seq (- end 2)))
                         (funcall-if key (aref seq (- end 1)))))))
             (loop for i from (- end (if init-p 2 3)) downto start
                   do (setf result
                            (funcall
                              fn
                              (funcall-if key (aref seq i))
                              result)))
             result)))))

(defun reduce-list (fn seq from-end start end key init init-p)
  (if (null end) (setf end (length seq)))
  (cond ((> start 0)
         (reduce-list fn (nthcdr start seq) from-end 0
                      (- end start) key init init-p))
        ((or (null seq) (eql start end))
         (if init-p init (funcall fn)))
        ((= (- end start) 1)
         (if init-p
             (funcall fn init (funcall-if key (first seq)))
             (funcall-if key (first seq))))
        (from-end
         (reduce-vect fn (coerce seq 'vector) t start end
                      key init init-p))
        ((null (rest seq))
         (if init-p
             (funcall fn init (funcall-if key (first seq)))
             (funcall-if key (first seq))))
        (t (let ((result
                   (if init-p
                       (funcall
                         fn init
                         (funcall-if key (pop seq)))
                       (funcall
                         fn
                         (funcall-if key (pop seq))
                         (funcall-if key (pop seq))))))
             (if end
                 (loop while seq
                    repeat (- end (if init-p 1 2))
                    do (setf result
                             (funcall
                               fn result
                               (funcall-if key (pop seq)))))
                 (loop while seq
                    do (setf result
                             (funcall
                               fn result
                               (funcall-if key (pop seq))))))
             result))))


;;; The Debugging Output Facility:
;;; debug functions based on "Paradigms of AI Programming" by Peter Norvig.
;;;
;;; The function "dbg" prints output in the same way as "format", but will only
;;; print when debugging output is desired.  Each call to dbg is accompanied by
;;; an identifier that is used to specify a class of debugging messages.
;;; The functions "dbg-on" and "dbg-off" are used to add or remove message
;;; classes to the list of classes that should be printed. 
;;;
;;; A call to dbg will result in output if the first argument, the
;;; identifier, is one that was specified in a call to dbg-on.  The other
;;; arguments are a format string followed by a list of arguments to
;;; be printed according to the format string.  In other words,
;;; functions will include calls to debug like:
;;;    (dbg :foo "The current goal is: ~a" goal)
;;; If debugging has been enabled with (dbg-on :foo), then calls to
;;; debug with the identifier :foo will print output.  The output is
;;; turned off with (debug-off :foo).
;;;
;;; Sometimes it is easier to view debugging output if it is indented according
;;; to some pattern, such as the depth of nested calls to a function.  To 
;;; generate indented output, the function debug-indent is used.

(defvar *dbg-ids* nil "Identifiers used by dbg.")

(defun DBG (id format-string &rest args)
  "Print debugging info if (DBG-ON ID) has been specified."
  (when (member id *dbg-ids*)
    (format *debug-io* "~&~?" format-string args)))

(defun DBG-INDENT (id indent format-string &rest args)
  "Print indented debugging info if (DBG-ON ID) has been specified."
  (when (member id *dbg-ids*)
    (format *debug-io* "~&~V@T~?" (* 2 indent) format-string args)))

(defun DBG-ON (&rest ids)
  "Start debug output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun DBG-OFF (&rest ids)
  "Stop debug on the ids.  With no ids, stop debug altogether."
  (setf *dbg-ids* (if (null ids)
		      nil
		    (set-difference *dbg-ids* ids))))
