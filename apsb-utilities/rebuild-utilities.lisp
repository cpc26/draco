
;;; Rebuild the utilities binaries.

(defpackage apsb-utilities
  (:use "COMMON-LISP")
  (:nicknames "APSB" "UTILITIES")
  (:export "APSB-LOGIN"))

(let* ((rel-1 (second (member "-release" (sys:command-line-arguments) :test #'string-equal)))
       (release (if rel-1
		    (intern (string-upcase rel-1) 'keyword))))
  (unless release
    (error "Command line argument -release not specified."))

  (format t "~%Rebuilding ~a utilities..." release)

  (load-logical-pathname-translations "apsb-lisp")
  (load "apsb-lisp:apsb-init")

  (apsb:apsb-login :systems 'apsb-utilities :release release)
  )
