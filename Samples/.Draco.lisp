;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(draco);-*-
;;;
;;;	Draco initialization file

(in-package draco)


(define-initialization-file
	:file			"Draco:Samples;file-types.lisp"
	:documentation		"file type specifications"
	:load-immediately	t
)
(define-report-keywords
	:reporter		FITS-reporter
	:keywords		(IMAGETYP  OBSERVAT  FILTERS   DATE-OBS
				 UT        RA        DEC)
)
(define-report-keywords
	:reporter		FITS-calib-reporter
	:keywords		(IMAGETYP  OBSERVAT  FILTERS   DATE-OBS UT)
)
(define-report-keywords
	:reporter		GEIS-reporter
	:keywords		(FILETYPE  IMAGETYP  INSTRUME  ROOTNAME
				 FILTNAM1  FILTNAM2  RA_TARG   DEC_TARG
				 DATATYPE  DATE-OBS  TIME-OBS)
)
(define-report-keywords
	:reporter		OIF-reporter
	:keywords		(IMAGETYP  OBSERVAT  FILTERS   DATE-OBS
				 UT        RA        DEC)
)
(define-report-keywords
	:reporter		OIF-calib-reporter
	:keywords		(IMAGETYP  OBSERVAT  FILTERS   DATE-OBS UT)
)
