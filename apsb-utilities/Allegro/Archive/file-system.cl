;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define C types for "stat".  These come from sys/types.h and sys/stdtypes.h.
;;; See also sys/stat.h

;; work around Allegro bug.
#! #+:Allegro (defparameter *intern-allows-symbol* excl:*intern-allows-symbol*)
#! #+:Allegro (setf excl:*intern-allows-symbol* t)			

#+:Allegro (ff:def-c-typedef dev_t :short)
#+:Allegro (ff:def-c-typedef ino_t :unsigned-long)
#+:Allegro (ff:def-c-typedef mode_t :unsigned-short)
#+:Allegro (ff:def-c-typedef uid_t :unsigned-short)
#+:Allegro (ff:def-c-typedef gid_t :unsigned-short)
#+:Allegro (ff:def-c-typedef off_t :long)
#+:Allegro (ff:def-c-typedef time_t :long)
#+:Allegro (ff:def-c-type stat :struct
			  (st_dev dev_t)
			  (st_ino ino_t)
			  (st_mode mode_t)
			  (st_nlink :short)
			  (st_uid uid_t)
			  (st_gid gid_t)
			  (st_rdev dev_t)
			  (st_size off_t)
			  (st_atime time_t)
			  (st_spare1 :int)
			  (st_mtime time_t)
			  (st_spare2 :int)
			  (st_ctime time_t)
			  (st_spare3 :int)
			  (st_blksize :long)
			  (st_blocks :long)
			  (st_spare4 :long))

#! #+:Allegro (setf excl:*intern-allows-symbol* *intern-allows-symbol*)


;; declarations
(declaim (function stat (string) stat))
(declaim (function file-size (string) off_t))
(declaim (function null-file (string) boolean))


(defun FILE-SIZE (filename)
  (aver (file-spec filename))
  "Return the size of the file, in bytes."
  (stat-st_size (stat filename)))


(defun NULL-FILE (filename)
  (aver (file-spec filename))
  "Return nil iff the file named FILENAME is empty."
  (zerop (file-size filename)))


#+:Allegro (load "" :unreferenced-lib-names (list (ff:convert-to-lang 'stat)))
#+:Allegro (ff:defforeign '|stat(2)| :entry-point (ff:convert-to-lang "stat"))

(defun STAT (filename)
  (aver (file-spec filename))
  "See stat(2).  Signal an error if appropriate.  E.g.,
       (stat-st_size (stat \"/etc/motd\"))
    returns the size of /etc/motd."
  (let ((buf (make-stat)))
    (unless (zerop (|stat(2)| (namestring filename) buf))
      (error (apsb:sys_errlist (apsb:errno))))
    buf))


(define-test-function stat
  ((file-size "/dev/null")	0)
  ((null-file ".")		nil)
)
