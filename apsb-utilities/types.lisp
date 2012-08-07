;;;-*-Mode:LISP; Syntax: Common-Lisp;Package:(apsb-utilities);-*-
;;;
;;;	frequently used type(-related) definitions
;;;

(in-package apsb-utilities)

#!(export '(boolean file-spec function-spec moniker optional stream-spec))
#!(export '(errno lockf-file-error shell-status-error text unix-error))

(deftype boolean () t)

(deftype file-spec () '(or pathname string))

(deftype function-spec () '(or function moniker))

(deftype moniker () '(and symbol (not null)))

(deftype optional (type) `(or ,type null))

(deftype stream-spec () '(or pathname stream string (eql t)))


;;; conditions

(define-condition unix-error (error)
  ((errno :reader errno :initarg :errno)
   (text :reader text :initarg :text)))

(define-condition lockf-file-error (unix-error file-error)
  ())

(define-condition shell-status-error (unix-error)
  ())
