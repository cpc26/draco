;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
(in-package Norvig)

;;; Code from Paradigms of AI Programming
;;; Copyright (c) 1991 Peter Norvig

;;; File auxmacs.lisp: Macros Used in other Auxiliary Functions
;;; Load this before anything else, then load auxfns.lisp.

;;; auxmacs.lisp,v
;;; Revision 1.1  1992/05/10  19:53:44  mrose
;;; Initial revision
;;;

;;; History:
;;; Apr 12 1992 mrose, changed package from USER to NORVIG

(defmacro once-only (variables &rest body)
  "Returns the code built by BODY.  If any of VARIABLES
  might have side effects, they are evaluated once and stored
  in temporary variables that are then passed to BODY."
  (assert (every #'symbolp variables))
  (let ((temps (loop repeat (length variables) collect (gensym))))
    `(if (every #'side-effect-free? (list .,variables))
         (progn .,body)
         (list 'let
               ,`(list ,@(mapcar #'(lambda (tmp var)
                                     `(list ',tmp ,var))
                                 temps variables))
               (let ,(mapcar #'(lambda (var tmp) `(,var ',tmp))
                             variables temps)
                 .,body)))))

(defun side-effect-free? (exp)
  "Is exp a constant, variable, or function,
  or of the form (THE type x) where x is side-effect-free?"
  (or (atom exp) (constantp exp)
      (starts-with exp 'function)
      (and (starts-with exp 'the)
           (side-effect-free? (third exp)))))

(defmacro funcall-if (fn arg)
  (once-only (fn)
    `(if ,fn (funcall ,fn ,arg) ,arg)))

(defmacro read-time-case (first-case &rest other-cases)
  "Do the first case, where normally cases are
  specified with #+ or possibly #- marks."
  (declare (ignore other-cases))
  first-case)
