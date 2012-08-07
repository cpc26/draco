;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	IRAF image list file recognizer
;;;
;;;	all Draco file type recognizer functions must have this
;;;	syntax:  ( <recognizer name>  <file pathname> )
;;;	and must return a string describing the file type when
;;;	they succeed, or nil.
;;;

(in-package draco)


;; internal function declaration
(declaim (function IRAF-image-list-p (file-spec) (optional string)))


;; local function definition
(flet
  ((IRAF-image-list-p1 (file-spec)
	(aver (file-spec file-spec))

	;; if each line in the file names a GEIS-calib-image file
	(let ((file-line nil))
	     (aver ((optional string) file-line))

	     (with-open-file (stream file-spec)
		(aver (stream stream))

		(loop

		  (setq file-line (read-line stream nil nil))

		  (when (null file-line)			; eof test
			(return "IRAF image list file"))	; success

		  ;; the first "line" of a file typically contains a
		  ;; lot of rubbish which can make the shell complain.
		  ;; strictly speaking, we should find a way to enable
		  ;; the user to use arbitrary file names, but this is
		  ;; just Sample code, so we won't.  instead . . .
		  (when (string= "" file-line)
			(return nil))		; it's definitely rubbish

		  (when (some #'(lambda (badchar)
					(aver (character badchar))
					(find badchar file-line))
			      '(#\space #\backspace #\tab
				#\linefeed #\page #\return
				#\rubout #\! #\* #\( #\) #\?))
			(return nil))		; it's probably rubbish

		  (unless (zerop (shell	(concatenate 'string
						(namestring *tools-dir*)
						"GEIS-p "
						file-line)
					:output			#p"/dev/null"
					:error 			:output
					:if-output-exists	:append
					:ignore-status		t))
			  (return nil)))))))	; it failed GEIS-p

	;; local function declaration
	(declare (function IRAF-image-list-p1 (file-spec) (optional string)))

	;; internal function definition
	(defun IRAF-image-list-p (file-spec)
		"IRAF image list file recognizer"
		(aver (file-spec file-spec))

		(and (not (zerop (run-shell-command	; directory test
				   (concatenate 'string
				     "test -d " (namestring file-spec)))))
		     (IRAF-image-list-p1 file-spec)))
)
