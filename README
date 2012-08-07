This is what Felix left:

to load Draco:

	start Common Lisp
	(make:load-system '%draco :compile-during-load t)


to compile Draco:

	(make:compile-system '%draco)

----------
notes added by glenn


;;;Felix did this, but this assumes something in the cl lib area:
;;;   (load-logical-pathname-translations "Draco")

(setf (logical-pathname-translations "Draco")
  '( 
    (";**;*.*"	"/lor/data1/Draco/")
    ("**;*.*"	"/lor/data1/Draco/")
    ))

(load "/lor/data1/Draco/apsb-utilities/CMU/defsystem.lisp")

(pushnew #p"Draco:" make:*central-registry*)

(pushnew #p"Draco:apsb-utilities" make:*central-registry*)

(make:load-system '%draco :compile-during-load t)

(in-package draco)

;;;; try these...

(set-data-directory "/lor/data1/scratch")

(sanity-test:start '(#p"."))

(excl:dumplisp :name "/lor/data1/draco.image")

;;; how do i build /lor/data1/draco/apsb-utilities/Allegro/unix-ff.o????

#|
First problem:

USER(10): (in-package draco)
#<The DRACO package>
DRACO(11): (set-data-directory "/lor/data1/scratch")
Error: Class #<STANDARD-CLASS FIRE @ #x71820a> is not yet finalized.
  [condition type: PROGRAM-ERROR]
  [1] DRACO(12): 


  Second problem:

  USER(6): USER(6): (sanity-test:start '(#p"."))
Error: "changing directory " resulted in error "No such file or directory"
[1] USER(7): 





|#
--------

notes for felix letter

Q. why declaim strings and fuctions? 
A. to allow compiler to optimize


