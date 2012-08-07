;;; -*- Mode: Lisp; Package: APSB-UTILITIES -*-

;;; Lisp access to crypt(3).
;;; Dec 10 1992 mrose

(in-package apsb-utilities)

#!(export '(crypt salt))

#+:Allegro (ff:defforeign 'crypt-3 :entry-point (ff:convert-to-lang "crypt"))

(defun CRYPT (key &optional (salt (if key (random-salt))))
  "Just like crypt(3)."
  (aver ((optional string) key salt))
  (if key
      #+:Allegro (ff:char*-to-string (crypt-3 key salt))
  ))

(defun SALT (password)
  "Return the salt from an encrypted password."
  (subseq password 0 2))

(defconstant salt-characters
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789./"
  "Make the salt out of these characters.")

(defun RANDOM-SALT ()
  "Return a random 2 character string suitable for use as a crypt(3) salt."
  (make-array 2
	      :element-type	'character
	      :initial-contents	(list (elt salt-characters (random 64))
				      (elt salt-characters (random 64)))))
