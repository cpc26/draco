;;; -*- Mode:Common-Lisp; Package:(make); Base:10 -*-
;;;
;;;	CMU defsystem hook

(in-package make)

(eval-when (:compile-toplevel :load-toplevel :execute)
	(export '(auto-doc-system)))


(defun auto-doc-operation (component force)
	"handles the :auto-doc operation for operate-on-system."
	(declare (type component component)
		 (ignore force))
	(when (probe-file (component-full-pathname component :source))
	      (with-tell-user ("Documenting source" component :source)
		(or *oos-test*
		    (apsb-utilities:auto-doc
			(component-full-pathname component :source))))))


(defun auto-doc-system (name
			&key force
			     (version *version*)
			     (test *oos-test*)
			     (verbose *oos-verbose*)
			     (load-source-instead-of-binary
				*load-source-instead-of-binary*)
			     (load-source-if-no-binary
				*load-source-if-no-binary*)
			     (bother-user-if-no-binary
				*bother-user-if-no-binary*)
			     (compile-during-load *compile-during-load*)
			     dribble
			     (minimal-load *minimal-load*))
	"convenience function for (operate-on-system <foo> :auto-doc ...)"
	(operate-on-system name :auto-doc
		:force				force
		:version			version
		:test				test
		:verbose			verbose
		:load-source-instead-of-binary	load-source-instead-of-binary
		:load-source-if-no-binary	load-source-if-no-binary
		:bother-user-if-no-binary	bother-user-if-no-binary
		:compile-during-load		compile-during-load
		:dribble			dribble
		:minimal-load			minimal-load))


(component-operation :auto-doc #'auto-doc-operation)
