;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(apsb-utilities);-*-
;;;
;;;	date utility *date-default-map* initialization
;;;

(in-package apsb-utilities)


(define-date-directive
	:directive	#\D
	:format		"~2,'0D/~2,'0D/~2,'0D"
	:args		(month date (rem year 100))
)

(define-date-directive
	:directive	#\H
	:format		"~2,'0D"
	:args		(hour)
)

(define-date-directive
	:directive	#\M
	:format		"~2,'0D"
	:args		(minute)
)

(define-date-directive
	:directive	#\S
	:format		"~2,'0D"
	:args		(second)
)

(define-date-directive
	:directive	#\T
	:format		"~2,'0D:~2,'0D:~2,'0D"
	:args		(hour minute second)
)

(define-date-directive
	:directive	#\a
	:format		"~A"
	:args		((day-string day-of-week))
)

(define-date-directive
	:directive	#\d
	:format		"~2,'0D"
	:args		(date)
)

(define-date-directive
	:directive	#\h
	:format		"~A"
	:args		((month-string month))
)

(define-date-directive
	:directive	#\m
	:format		"~2,'0D"
	:args		(month)
)

(define-date-directive
	:directive	#\t
	:format		"~8,8T"
	:args		()
)

(define-date-directive
	:directive	#\w
	:format		"~1D"
	:args		(day-of-week)
)

(define-date-directive
	:directive	#\y
	:format		"~4D"
	:args		(year)
)


(define-test-function date
	"testing apsb-utilities:date, not the usual usual"
	((date :time 2916510645 :format "~D ~H ~M ~S ~T ~a ~d ~h ~m ~t ~w ~y")
		"06/02/92 17 50 45 17:50:45 Tue 02 Jun 06         1 1992")
	((prog1 t
	   (format *diagnostic-stream* "~&~A"
	     (date :format "~~T~t~t~~H~t~~M~t~~S~%~T~t~H~t~M~t~S"))
	   (format *diagnostic-stream* "~&~A"
	     (date :format "~%~~D~t~t~~m~t~~h~t~~d~t~~y~%~D~t~m~t~h~t~d~t~y"))
	   (format *diagnostic-stream* "~&~A"
	     (date :format "~%~~a(~~w)~%~a(~w)")))
	t)
)
