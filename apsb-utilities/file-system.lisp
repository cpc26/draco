;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(apsb-utilities);-*-
;;;
;;;	file system utilities
;;;

(in-package apsb-utilities)

#!(export '(clear-fn copy-stream-to-file copy-file doc-string file file-size fire
	    fire-clear fire-copy fire-load fire-query fire-update init-fn
	    load-fn load-parameters make-fire null-file reload-parameters
	    synch-time update-fn update-parameters))
;;;
;;;	FIRE (file repository) utility
;;;	      ~~   ~~
;;;
;;;	Synopsis:
;;;
;;;	  [having always hated block comments, i've always wanted to
;;;	  substitute the lyrics to the theme from Gilligan's Island.
;;;	  alas, not today.  -- fwy]
;;;
;;;	  the FIRE utility enables one to associate arbitrary internal
;;;	  representations, e.g. hash tables and alists, with external
;;;	  repositories, e.g. files and streams.  these associations
;;;	  are represented by instances of the FIRE class.
;;;
;;;	  an instance of FIRE is essentially a read-only data structure,
;;;	  i.e. once it is created, it is usually modified only by the
;;;	  FIRE utility.  the instance therefore associates the specified
;;;	  internal representation with a particular file or stream.
;;;	  we provide the following operators: fire-copy, fire-load,
;;;	  fire-query, fire-update, and make-fire.
;;;
;;;	  fire-copy (method) creates and returns a copy of an instance
;;;	  of FIRE.  the returned instance may associate a different
;;;	  file/stream with the internal representation.
;;;
;;;	  fire-load (method) loads the FIRE's file/stream using the
;;;	  FIRE's load-fn and the appropriate (re)load-parameters.
;;;
;;;	  fire-query (macro) insures that the internal representation
;;;	  is at least as new as its associated file/stream before
;;;	  invoking the specified query.
;;;
;;;	  fire-update (method) overwrites the FIRE's file/stream with
;;;	  the contents of the internal representation.
;;;
;;;	  make-fire (function) creates and returns an instance of FIRE.
;;;
;;;
;;;	Default Behavior:
;;;
;;;	  fire-update signals an error unless an update-fn has been
;;;	  specified.  in other words, file/stream repositories are
;;;	  read-only by default.
;;;
;;;	  init-fn and clear-fn both default to nil.  in other words, the
;;;	  internal representation is assumed to be self-(re)initializing,
;;;	  i.e. the act of "loading" the forms in the file repository
;;;	  should create/clear the internal representation by default.
;;;
;;;
;;;	Creating New Repositories:
;;;
;;;	  since FIRE instances are basically read-only objects, associating
;;;	  a new file/stream with an internal representation is not a trivial
;;;	  process.  one cannot simply (setf (file ...) ...).
;;;
;;;	  there are two scenarios to consider: (1) the internally represented
;;;	  data are deemed superior to the repository's data, but the user
;;;	  does not wish to destroy the current repository, i.e. invoke the
;;;	  update-fn, and (2) a new repository is deemed superior to the
;;;	  current one and we wish to replace the internally represented data
;;;	  with the new repository's data.
;;;
;;;	  in both cases, the correct approach is to create a second FIRE
;;;	  instance, perhaps via the fire-copy method.  the internal
;;;	  representation can then be written out (via the second instance's
;;;	  update-fn) or refreshed (the first time fire-query is invoked on
;;;	  the second instance).  the application is responsible for any
;;;	  bookkeeping that may be necessary.
;;;

;; declaration
(declaim (function make-fire (stream-spec &rest args
			      &key documentation init-fn load-fn
				   load-parameters reload-parameters
				   clear-fn update-fn)
			     fire))


;; exported class definitions
(defclass fire ()
	(
	 (file
		:type		stream-spec
		:documentation	"specifies the external file repository"
		:initform	(error *missing-keyword-format*
				  "(make-instance 'FIRE ...)" :file)
		:initarg	:file
		:reader		file)
	 (documentation
		:type		(optional string)
		:documentation	"description of the data contained in FILE"
		:initform	nil
		:initarg	:documentation
		:reader		doc-string)
	 (init-fn
		:type		function-spec
		:documentation	"initializes the internal representation"
		:initform	#'(lambda ())
		:initarg	:init-fn
		:accessor	init-fn)
	 (load-fn
		:type		function-spec
		:documentation	"(funcall LOAD-FN FILE) reads FILE into memory"
		:initform	#'load
		:initarg	:load-fn
		:accessor	load-fn)
	 (load-parameters
		:type		list
		:documentation	"a keyword-value plist for LOAD-FN's 1st call"
		:initform	nil
		:initarg	:load-parameters
		:accessor	load-parameters)
	 (reload-parameters
		:type		list
		:documentation	"like LOAD-PARAMETERS but for subsequent calls"
		:initform	nil
		:initarg	:reload-parameters
		:accessor	reload-parameters)
	 (synch-time
		:type		(optional integer)
		:documentation	"last time FILE was known to be up-to-date"
		:initform	nil	; nil -> FILE was never loaded
		:accessor	synch-time)
	 (clear-fn
		:type		function-spec
		:documentation	"clears the internal representation"
		:initform	#'(lambda ())
		:initarg	:clear-fn
		:accessor	clear-fn)
	 (update-fn
		:type		(optional function-spec)
		:documentation	"(funcall UPDATE-FN FILE) updates FILE"
		:initform	nil
		:initarg	:update-fn
		:accessor	update-fn)
	 (update-parameters
		:type		list
		:documentation	"a keyword-value plist for UPDATE-FN calls"
		:initform	nil
		:initarg	:update-parameters
		:accessor	update-parameters)
	)
	(:documentation "
	associates an external repository (a file or
	a stream) with its internal representation")
)

(defmethod print-object ((fire fire) (stream stream))
	(print-unreadable-object (fire stream :type t :identity t)
		(princ (or (doc-string fire) (file fire)) stream)))

(defmethod shared-initialize :before ((fire fire) slot-names &rest initargs)
	"discourages reinitialization of (file fire)"
	(declare (ignore slot-names))
	(aver (list initargs))
	(when (slot-boundp fire 'file)
	      (unless (equalp (file fire) (getf initargs :file))
		      (error "attempt to redefine (file ~a)" fire))))


;; exported class-specific method definitions
(defmethod fire-clear ((fire fire))
	"invoke (clear-fn fire)"
	(unless (fboundp (clear-fn fire))
		(error "~a has no clear-fn" fire))
	(funcall (clear-fn fire)))


(defmethod fire-copy ((fire fire) &key (file (file fire)))
	"return a copy of fire with (file fire) initialized to file"
	(aver (stream-spec file))
	(make-fire file
	  :documentation	(doc-string fire)
	  :init-fn		(init-fn fire)
	  :load-fn		(load-fn fire)
	  :load-parameters	(load-parameters fire)
	  :reload-parameters	(reload-parameters fire)
	  :clear-fn		(clear-fn fire)
	  :update-fn		(update-fn fire)
	  :update-parameters	(update-parameters fire)
	))


(defmethod fire-load ((fire fire) &key (force nil))
	"
	load or reload (file fire) if necessary or if :force is non-nil.
	return (file fire)'s file-write-date if it was loaded or reloaded,
	and nil if no load was performed.  :force defaults to nil.
	a non-nil value is returned if the load was successfully performed;
	if no load was necessary, nil is returned.  if an error occured
	while attempting the load, two values are returned: nil and the
	error (condition)."
	(aver (boolean force))
	(with-accessors ((file file)
			 (init-fn init-fn)
			 (load-fn load-fn)
			 (load-parameters load-parameters)
			 (reload-parameters reload-parameters)
			 (synch-time synch-time)
			 (clear-fn clear-fn)) fire
	  (ignore-errors
		(etypecase synch-time
		  (null		(funcall init-fn)
				(setf synch-time (file-write-date file))
				(apply load-fn file load-parameters))
		  (integer	(if (or force
					(< synch-time (file-write-date file)))
				    (then (funcall clear-fn)
					  (setf synch-time
						(file-write-date file))
					  (apply load-fn
						 file reload-parameters))))
		))))


(defmethod fire-update ((fire fire))
	"
	invoke (update-fn fire) and return the new file-write-date
	of (file fire); if (update-fn fire) does not exist, an error
	is signaled."
	(unless (update-fn fire) (error "~a has no update-fn" fire))
	(apply (update-fn fire) (file fire) (update-parameters fire))
	(setf (synch-time fire) (file-write-date (file fire))))


;; exported macro definition
(defmacro fire-query (fire query)
	"load or reload (file fire), if necessary, before invoking query"
	`#!(progn (fire-load ,fire) ,query))


;; exported function definitions
(defun copy-stream-to-file (stream file)
  (with-open-file (file-stream file :direction :output)
    (do ((line (read-line stream nil nil) (read-line stream nil nil)))
	((null line) file)
      (write-line line file-stream))))

(defun copy-file (source dest)
  (with-open-file (source-stream source :direction :input)
    (copy-stream-to-file source-stream dest)))

(defun file-size (file-spec &key (element-type :default))
	"
	return size of file-spec;
	return nil if file-spec does not name a file that can be opened"
	(aver (file-spec file-spec))
	(ignore-errors
		(with-open-file (stream file-spec :element-type element-type)
			(file-length stream))))


(defun null-file (file-spec)
	"return nil unless file-spec names an empty file"
	(aver (file-spec file-spec))
	(zerop (file-size file-spec)))


(defun make-fire (stream-spec &rest args
		  &key documentation init-fn load-fn load-parameters
		       reload-parameters clear-fn update-fn)
	"create and return an instance of FIRE"
	(declare (ignore documentation init-fn load-fn load-parameters
			 reload-parameters clear-fn update-fn))
	(aver (stream-spec stream-spec)
	      (list args))
	(apply #'make-instance 'fire :file stream-spec args))


(define-test-function file-size
  ((file-size "/dev/null")      0)
)

(define-test-function null-file
  ((null-file ".")              nil)
)
