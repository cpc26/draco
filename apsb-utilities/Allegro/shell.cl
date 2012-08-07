;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(apsb-utilities);-*-
;;;
;;;	shell definitions
;;; Modified: dja - 24 May 93

(in-package apsb-utilities)

#!(intern "RUN-SHELL-COMMAND")
#!(intern "SHELL")
#!(defpackage user
	(:shadowing-import-from "APSB-UTILITIES"
		"RUN-SHELL-COMMAND"
		"SHELL"
	)
)
#!(export '(getenv run-shell-command shell
	    Blocking-Wait-With-PID Non-Blocking-Wait-With-PID
	    Process-Finished-p))

;; declarations
(declaim (function getenv (string) string))
(declaim (function run-shell-command
		   (&rest list) (or null stream) (or null stream) integer))

(declaim (inline getenv run-shell-command))		; speed kills?

;;; ----------------------------------------------------------------
;;;               *** Foreign Function Declarations ***
;;; ---------------------------------------------------------

#+:Allegro (ff:defforeign 'Blocking-Wait-With-PID 
		:arguments '(fixnum)
		:entry-point (ff:convert-to-lang "Blocking_Wait_With_PID")
		:return-type :integer)

#+:Allegro (ff:defforeign 'Non-Blocking-Wait-With-PID 
		:arguments '(fixnum)
		:entry-point (ff:convert-to-lang "Non_Blocking_Wait_With_PID")
		:return-type :integer)

;; exported function definitions
(defun getenv (var)
	"C (stdlib) getenv"
	(aver (string var))
	#+:Allegro (system:getenv var)
	#-:Allegro (break "wake up")
)


(defun Process-Finished-p (Process-ID)
  "Return t if the process with id PROCESS-ID has finished executing
and is waiting to signal its parent that its done.  Else, return nil.
This takes advantage of non-blocking status queries.  This is much
better than Allegro's sys:os-wait."
  (let (Result)
    (setf Result (Non-Blocking-Wait-With-PID Process-ID))
    (cond ((= Result -1) (values :success))
	  ((= Result 0) (values :not-finished))
	  ((> Result 0) (values :failure Result))
	  (t (values :not-finished)))))
			
(defun run-shell-command (&rest args)
	"Allegro-inspired shell command interface"
	(aver (list args))
	#+:Allegro (apply 'excl:run-shell-command args)
	#-:Allegro (break "wake up")
)

;;; Feb 26 1993 mrose

(defun SHELL (command
	      &key (input #p"/dev/null")
		   (output *standard-output*)
		   (error *error-output*)
		   (if-output-exists :error)
		   (if-error-output-exists :error)
		   (ignore-status nil)
		   )
  "Run the specified unix shell command.  Wait for completion.

COMMAND is a string.

INPUT may be a stream, pathname, string, or sequence of strings.
If a stream, that stream is read, copied to a file, and that file
is used as input for the command.  If a pathname, that file is used
as input for the command.  If a sequence of strings, the strings are
written to a temporary file, a newline inserted after each string,
and the file is used as the input.  A single string is treated as a
sequence of one string.

OUTPUT may be a pathname, a lisp stream, T, or nil.  If a pathname,
the output is written to that file.  If a stream, the output from
the command is written first to a temporary file, then to the
stream.  T is shorthand for *standard-output*.  If nil, the output
is written to a temporary file, then returned as a list of strings.

Same for ERROR, except it may have the additional value :OUTPUT,
which directs ERROR to the same place as OUTPUT.

Signal an error if the command returns non-zero status and
IGNORE-STATUS is nil.  The text of the error is taken from ERROR.

The keywords :IF-OUTPUT-EXISTS and :IF-ERROR-OUTPUT-EXISTS behave
like OPEN's :IF-EXISTS keyword.  The default action for each is
to signal an error.

Return 3 values: the completion status, output-related-info, and
error-related-info.  If OUTPUT is a pathname or stream,
output-related-info is nil; if OUTPUT is nil, output-related-info
is a list of strings that are the actual output.  Error-related-info
is just like output-related-info.  When error is :output,
error-related-info is nil."

  (let* ((tag (gentemp))
	 (input-filename (etypecase input
			   (pathname (namestring input))
			   (string (write-strings-to-file (list input) (apsb:tempnam :tag tag)))
			   (sequence (write-strings-to-file input      (apsb:tempnam :tag tag)))
			   (stream (copy-stream-to-file   input        (apsb:tempnam :tag tag)))))
	 (output-filename (etypecase output
			    (pathname (namestring output))
			    ((or stream null (eql t)) (apsb:tempnam :tag tag))))
	 (error-filename (etypecase error
			   (pathname (namestring error))
			   ((or stream null (eql t)) (apsb:tempnam :tag tag))
			   ((eql :output) :output))))
    (unwind-protect
	(let ((status (run-shell-command
			command
			:wait t
			:input input-filename
			:output output-filename
			:error-output error-filename
			:if-output-exists if-output-exists
			:if-error-output-exists if-error-output-exists)))
	  (unless (or (zerop status)
		      ignore-status)
		  (error 'shell-status-error
			:format-control	    "~@{~a~%~}" 
			:format-arguments (if (eq error :output)
					      "<error text unavailable>"
					    (file-strings error-filename))
			:errno		    status
			:text		    "Unexpected nonzero status code"))
	  (when (or (streamp output)
		    (eq output t))
	    (format output "~{~a~%~}" (file-strings output-filename)))
	  (when (or (streamp error)
		    (eq error t))
	    (format error "~{~a~%~}" (file-strings error-filename)))
	  (values status
		  (if (null output) (file-strings output-filename))
		  (if (null error) (file-strings error-filename))))
      ;; delete temp files
      (mapc #'(lambda(pathname)
		(when (probe-file pathname)
		  (delete-file pathname)))
	    (apsb:tempnam-output :tag tag)))))


(defun WRITE-STRINGS-TO-FILE (sequence &optional (filename (apsb:tempnam)))
  "Write a sequence to a file, separating each element with a newline,
return the filename."
  (with-open-file (file-stream filename :direction :output)
    (shell (concatenate 'string "chmod 600 " filename))  
    (map nil #'(lambda (elt)
		 (write-line elt file-stream))
	 sequence)
    filename))

(defun FILE-STRINGS (filename)
  "Return a list of the strings, each string is one line from FILENAME."
  (collect (scan-file filename #'read-line)))

;;; Test the following features of SHELL
;;; 1. :input from
;;;       a. /dev/null
;;;       b. named file
;;;       c. string
;;;       d. sequence of strings
;;; 2. :output, :error
;;;       a. nil,  nil
;;;       b. nil,  /dev/null"
;;;       c. nil,  :output
;;;       d. /dev/null,  nil
;;;       e. /dev/null,  :output
;;;       f. named file, named file
;;;       g. stream,  stream
;;;

(defun perl-script (status)
	"create and return perl script/string"
	(aver (fixnum status))
	(format nil
		"perl -e 'print STDOUT \"stdout1\\nstdout2\\n\";print STDERR \"stderr\\n\";exit ~a'"
		status))

(define-test-function shell
    "Test the SHELL function."

    ;; 1a, 2a
    ((apsb:shell (perl-script 99) :output nil :error nil :ignore-status t)
     99 '("stdout1" "stdout2") '("stderr"))

    ;; 1b
    ((let ((stdin (pathname (write-strings-to-file '("65535*65535")))))
	  (multiple-value-prog1
	    (apsb:shell "bc" :input stdin :output nil :error nil)
	    (when (probe-file stdin) (delete-file stdin))))
     0 '("4294836225") NIL)

    ;; 1c
    ((apsb:shell "bc" :input "32767*32767" :output nil :error nil)
     0 '("1073676289") NIL)
  
    ;; 1d
    ((apsb:shell "perl"
		 :input '("print STDOUT \"stdout3\\n\";"
			  "print STDOUT \"stdout4\\n\";"
			  "print STDERR \"stderr2\\n\";")
		 :output nil :error nil)
     0 '("stdout3" "stdout4") '("stderr2"))

    ;; 2b
    ((apsb:shell (perl-script 98)
		 :output nil :error #p"/dev/null" :ignore-status t
		 :if-error-output-exists :overwrite)
     98 '("stdout1" "stdout2") nil)

    ;; 2c
    ((multiple-value-bind (status stdout stderr)
       (apsb:shell (perl-script 97)
		   :output nil :error :output :ignore-status t)
       (values status (sort stdout #'string<) stderr))
     97 '("stderr" "stdout1" "stdout2") nil)

    ;; 2d
    ((apsb:shell (perl-script 96)
		 :output #p"/dev/null" :error nil :ignore-status t
		 :if-output-exists :overwrite)
     96 nil '("stderr"))

    ;; 2e
    ((apsb:shell (perl-script 95)
		 :output #p"/dev/null" :error :output :ignore-status t
		 :if-output-exists :overwrite)
     95 nil nil)

    ;; 2f
    ((let ((stdout-file (pathname (apsb:tempnam)))
	   (stderr-file (pathname (apsb:tempnam))))
       (multiple-value-prog1
	 (values
	   (multiple-value-list
	     (apsb:shell (perl-script 94)
	       :output stdout-file :error stderr-file :ignore-status t))
	   (file-strings stdout-file)
	   (file-strings stderr-file))
	 (when (probe-file stdout-file) (delete-file stdout-file))
	 (when (probe-file stderr-file) (delete-file stderr-file))))
     '(94 NIL NIL) '("stdout1" "stdout2") '("stderr"))

    ;; 2g
    ((let ((stdout-file (apsb:tempnam))
	   (stderr-file (apsb:tempnam)))
       (multiple-value-prog1
	 (values
	   (with-open-file (stdout stdout-file :direction :output)
	     (with-open-file (stderr stderr-file :direction :output)
	       (multiple-value-list
		 (apsb:shell (perl-script 93)
			     :output stdout :error stderr :ignore-status t))))
	   (file-strings stdout-file)
	   (file-strings stderr-file))
	 (when (probe-file stdout-file) (delete-file stdout-file))
	 (when (probe-file stderr-file) (delete-file stderr-file))))
     '(93 NIL NIL) '("stdout1" "stdout2") '("stderr"))
) ; define-test-function

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
