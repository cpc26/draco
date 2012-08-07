;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(apsb-utilities);-*-
;;;
;;;	file-system definitions
;;; Modified: dja - 30 July 1993

(in-package apsb-utilities)

#!(export '(cwd mkdir rmdir tempnam tempnam-output with-open-temp-file
	    umask With-Locked-File Get-File-Lock Release-File-Lock 
	    With-File-Resource
	    mp-fire make-mp-fire))


;; declarations
(declaim (function cwd (&optional file-spec) pathname))
(declaim (function tempnam (&key directory prefix tag) string))
(declaim (function tempnam-output (&key tag) list))

;;; ----------------------------------------------------------------
;;;               *** Foreign Function Declarations ***
;;; ---------------------------------------------------------

#+:Allegro (ff:defforeign '_tempnam
		:arguments '(string string)
		:entry-point (ff:convert-to-lang "tempnam")
		;:return-type :string
	   )

#+:Allegro (ff:defforeign 'unix-close
		:arguments '(fixnum)
		:entry-point (ff:convert-to-lang "close")
		:return-type :integer)
    
#+:Allegro (ff:defforeign 'unix-open
		:arguments '(string fixnum fixnum)
		:entry-point (ff:convert-to-lang "open")
		:return-type :integer)
    
#+:Allegro (ff:defforeign 'unix-umask
		:arguments '(fixnum)
		:entry-point (ff:convert-to-lang "umask")
		:return-type :integer)

;;; (get-file-lock <file-name> <create-flag> <non-blocking flag>)
;;; Returns -1 if fails or > 0 if succeeds.  If fails, check errno.
;;; Keep successful return value in order to call release-file-lock
;;; Flag map
;;;      Create Non-Blocking   Result
;;;      ----------------------------
;;;        0        0          if file exists, block until get lock
;;;        0        1          if file exists, see if can get lock
;;;                              but don't block if can't get it
;;;        1        0          if file does not exist, create it.
;;;                              block while wait for lock
;;;        1        1          if file does not exist, create it.
;;;                              but don't block if can't get lock
#+:Allegro (ff:defforeign 'get-file-lock
	       :entry-point (ff:convert-to-lang "Get_File_Lock")
	       :arguments '(string fixnum fixnum)
	       :return-type :integer)

#+:Allegro (ff:defforeign 'release-file-lock
	       :entry-point (ff:convert-to-lang "Release_File_Lock")
	       :arguments '(fixnum)
	       :return-type :integer)

;;; ----------------------------------------------------------------
;;;             *** Variables and Constants ***
;;; ----------------------------------------------------------------

(defvar *File-Mode* nil
  "*File-Mode* holds the value of the mode flags that new files will be created with.")

(defconstant *lockf-error-format-control* "Lockf failed for ~a.  errno=~d: ~a")

;;;
;;; From /usr/include/sys/fcntlcom.h
;;;

(defconstant *O_RDONLY* 0)
(defconstant *O_WRONLY* 1)
(defconstant *O_RDWR* 2)

(defconstant *O_TRUNC* #x0400) 
(defconstant *O_APPEND* #x0008)
(defconstant *O_CREAT* #x0200)

;; exported function definitions
(defun cwd (&optional (directory (excl:current-directory)))
	"
	the current working directory is returned as a string.
	if a directory is specified, it becomes the current working
	directory."
	(aver (file-spec directory))
	#+:Allegro (progn (excl:chdir directory) (excl:current-directory))
	#-:Allegro (break "wake up")
)


(let ((*tempnam-output* nil))
     (aver (list *tempnam-output*))

     (defun tempnam (&key (directory "") (prefix "") (tag nil))
	"
	Lisp interface to the C library routine \"tempnam\".
	Returns a string."
	(aver (string directory prefix)
	      (symbol tag))
	(unless (assoc tag *tempnam-output*)
		(push (list tag) *tempnam-output*))
	(car (push #+:Allegro (ff:char*-to-string (_tempnam directory prefix))
		   (cdr (assoc tag *tempnam-output*)))))


     (defun tempnam-output (&key (tag nil))
	"return list of temporary file pathnames"
	(aver (symbol tag))
	(mapcar #'pathname (cdr (assoc tag *tempnam-output*))))


     (defmethod (setf tempnam-output) (new-tempnam-output &key (tag nil))
	(aver (symbol tag))
	(unless (assoc tag *tempnam-output*)
		(push (list tag) *tempnam-output*))
	(setf (cdr (assoc tag *tempnam-output*))
	      new-tempnam-output))
)

(defmacro WITH-OPEN-TEMP-FILE ((var) . body)
  "Bind VAR to a stream open to a temporary file, run body,
then delete the temporary file."
  (aver (symbol var))
  (let ((filename (gensym)))
    `#!(let ((,filename (tempnam)))
	 (unwind-protect
	     (with-open-file (,var ,filename
			      :direction :output :if-exists :error)
	       ,@body)
	   (when (probe-file ,filename)
	     (delete-file ,filename))))))

(defun MKDIR (directory)
  (apsb::shell (format nil "mkdir ~a" directory))
  directory)

(defun RMDIR (directory)
  (apsb::shell (format nil "rmdir ~a" directory)))

;;; ----------------------------------------------------------------

(defun UMASK (&optional (Mask nil))
  "Make a call to unix's umask function.  This is necessary to set up
the file creation mode correctly.  DO NOT REMOVE.  In order to set the
file creation mode correctly, do (umask).  If you want to set a new
umask, do (umask <new mask>).  This call returns the previous umask
value."
  (aver ((optional integer) Mask))
  (let ((Old-Mask (unix-umask (or Mask #o0))))
    (unless Mask
	    (umask Old-Mask))
    Old-Mask))

(defun Calculate-File-Mode ()
  (setf *File-Mode* (logxor (umask) #o666)))

;;; ----------------------------------------------------------------

(defmacro With-Locked-File ((Stream File &rest Access-Info) &rest Body)

  "Like with-open-file, except file is locked when openned.  This is 
a cooperative locking.  Processes not using these locking function will
have access to the file without hamper.  Locking requires one to have
write access to the file that is to be locked.

   File locking is a blocking call in UNIX.  Ergo, if the file is locked 
when you request it, your process will block until the lock is released.
If you do not wish to block, include a :blocking :no in the arguments to
with locked file.  Blocking will be the default.  Examples are below.

  Blocking by default:  (with-locked-file (In File :direction :input) ...)
  Blocking: (with-locked-file (In File :direction :output :blocking :yes) ...)
  No blocking: (with-locked-file (In File :direction :input :blocking :no) ...)"

  (let ((block-info-provided (gensym))
	(block? (gensym))
	(errno (gensym))
	(fd (gensym))
	(text (gensym)))
    `#!(let ((,block-info-provided (find :blocking (quote ,Access-Info)))
	     (,block? t))

	 (when ,block-info-provided
	   (setf ,block? (not (eq :no (getf (quote ,Access-Info) :blocking)))))

	 (setf ,fd
	       (get-file-lock (namestring (translate-logical-pathname ,File))
			      ,(if (find :create Access-Info) 1 0)
			      (if ,block? 0 1)))
	 (unless (> ,fd 0)
	   (when ,block?
	     (let ((,errno (get-errno))
		   (,text (perror)))
		  (error 'lockf-file-error
		    :format-control	*lockf-error-format-control*
		    :format-arguments	(list ,File ,errno ,text)
		    :pathname		,File
		    :errno		,errno
		    :text		,text))))
	 (unwind-protect
	     (with-open-file (,Stream
			      ,File
			      ,@(progn
				  (remf Access-Info :blocking) Access-Info))
	       ,@Body)
	   (release-file-lock ,fd)))))

;;; ----------------------------------------------------------------

(defmacro With-File-Resource ((File Blocking?) &rest Body)
  "Uses file locking in order to control a resource."
  (let ((errno (gensym))
	(fd (gensym))
	(text (gensym)))
    `#!(let ((,fd (get-file-lock
		    (namestring (translate-logical-pathname ,File))
		    0
		    (if ,Blocking? 0 1))))
	 (unless (> ,fd 0)
	   ;; There was an error, and the lock was not seized
	   (let ((,errno (get-errno))
		 (,text (perror)))
	     (error 'lockf-file-error
	       :format-control		*lockf-error-format-control*
	       :format-arguments	(list ,File ,errno ,text)
	       :pathname		,File
	       :errno			,errno
	       :text			,text)))
	 (unwind-protect
	     (progn ,@Body)
	   (release-file-lock ,fd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIRE - specialization for multiple lisp sibling processes

(defclass MP-FIRE (fire)
  ((lock :type mp:process-lock
	:documentation "synchronizes multi-process access to file."
	:initform (mp:make-process-lock :name "fire")
	:initarg :lock
	:reader lock)))

(defun MAKE-MP-FIRE (stream-spec &rest args
		     &key documentation init-fn load-fn load-parameters
			  reload-parameters clear-fn update-fn lock)
  "create and return an instance of FIRE"
  (declare (ignore documentation init-fn load-fn load-parameters
		   reload-parameters clear-fn update-fn lock))
  (aver (stream-spec stream-spec)
	(list args))
  (apply #'make-instance 'mp-fire :file stream-spec args))


(defmethod FIRE-LOAD :around ((mp-fire mp-fire) &key (force nil))
  (declare (ignore force))
  (mp:with-process-lock ((lock mp-fire))
    (call-next-method)))

(defmethod FIRE-UPDATE :around ((mp-fire mp-fire))
  (mp:with-process-lock ((lock mp-fire))
    (call-next-method)))

(defmethod FIRE-COPY ((fire mp-fire) &key (file (file fire)))
  "return a copy of fire with (file fire) initialized to file"
  (aver (stream-spec file))
  (make-mp-fire file
		:documentation	        (doc-string fire)
		:init-fn		(init-fn fire)
		:load-fn		(load-fn fire)
		:load-parameters	(load-parameters fire)
		:reload-parameters	(reload-parameters fire)
		:clear-fn		(clear-fn fire)
		:update-fn		(update-fn fire)
		:update-parameters	(update-parameters fire)
		:lock                   (lock fire)
		))
