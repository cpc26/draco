;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(apsb-utilities);-*-
;;;
;;;	string utilities
;;;

(in-package apsb-utilities)

#!(export '(iswhite))


;; declaration
(declaim (function iswhite (character) boolean))


;; exported function
(defun iswhite (character)
	"returns T if character is whitespace; returns nil otherwise"
	(aver (character character))
	(find character '(#\newline #\space #\page #\tab #\return #\linefeed)
	      :test #'char=))
