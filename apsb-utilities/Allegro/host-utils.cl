;;; -*- Mode: Lisp; Package:(apsb-utilities)-*-

(in-package apsb-utilities)

#!(export '(host-name
	    host-address
	    host-aliases
	    server-name
	    server-address
	    internet-address-p
	    *hostname-defaults*
	    hostcase
	    ehostcase
	    chostcase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains functions for obtaining and manipulating
;;; internet address and host names.
;;;
;;; There are six exported functions.  Five accept a single optional argument
;;; "host-id".  This is either a string, an internet address (see below), or
;;; a symbol.  Other types can be defined with a new GET-HOST method.
;;; These functions, with arglists, are
;;;   HOST-NAME	     (&optional host-id)
;;;   HOST-ADDRESS   (&optional host-id)
;;;   HOST-ALIASES   (&optional host-id)
;;;   SERVER-NAME    (&optional host-id)
;;;   SERVER-ADDRESS (&optional host-id)
;;;
;;; Another function, that requires one argument of any type, is a predicate
;;; testing if the argument is of type internet-address.
;;;   INTERNET-ADDRESS-P (object)
;;;
;;; One variable is exported.
;;;   *HOSTNAME-DEFAULTS*
;;;
;;; Three macros are exported.  With arglists, these are
;;;   HOSTCASE  (keyform &rest clauses)
;;;   EHOSTCASE (keyform &rest clauses)
;;;   CHOSTCASE (keyform &rest clauses)
;;;
;;; A reader macro is provided for simple entry of addresses.
;;; A form like
;;;   #[130.167.107.10]
;;; creates and returns an object of type internet-address.
;;; Internally internet addresses are represented as a 32-bit integer.
;;; For example, #[130.167.107.10] is equivalent to #[2192009994] because
;;; 130 = #x82, 167 = #xA7, 107 = #x6B, 10 = #x0A, and 2192009994 = #x82A76B0A 
;;;
;;; Some typical calls are:
;;;    (host-address "prufrock")
;;;    (host-name #[130.167.104.14])
;;;    #[130.167.104.14]
;;;    (server-address (host-address "kali"))
;;;    (host-name (server-address #[130.167.104.14]))
;;;    (server-name)
;;;    (host-aliases "stsci.edu")
;;;    (hostcase "axolotl.stsci.edu"
;;;      ("axolotl.stsci.edu" "Thank you for flying Axolotl air")
;;;      (kali "The Goddess welcomes you.")
;;;      ((prufrock.stsci.edu marian) "You have chosen prufrock or marian")
;;;      (t "no preference, eh?"))
;;;    (ehostcase (server-name)
;;;      ("cerberus" "this is cerb")
;;;      ("marian" "this is marian"))
;;;
;;; Sep 11 1992 mrose
;;; Sep 16 1992 mrose, HOST-NAME, HOST-ADDRESS, SERVER-NAME, and SERVER-ADDRESS
;;;                    all accept a host name string, an internet address, a
;;;                    symbol, or nothing.  Other types may be defined by new
;;;		       GET-HOST methods.  A host of NIL returns nil, i.e. it
;;;		       does not default to the current system.  No argument to
;;;		       these functions defaults to (short-site-name).
;;;                    Don't export MAKE-INTERNET-ADDRESS.
;;;                    Create HOST-ALIASES.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *af_inet* 2
  "from /usr/include/sys/socket.h"
)
(defconstant *number-address-bytes* 4
  "number of bytes in an internet-address"
)

(defun HOST-NAME (&optional (host-id (short-site-name)))
  "Return the name of a host, or nil if it isn't in /etc/hosts.
The argument may be a string, a symbol, or an internet address;
the default is (short-site-name)."
  (name (get-host host-id)))

(defun HOST-ADDRESS (&optional (host-id (short-site-name)))
  "Return the internet address of a host, or nil if it isn't in /etc/hosts.
The argument may be a string, a symbol, or an internet address;
the default is (short-site-name)."
  (internet-address (get-host host-id)))

(defun HOST-ALIASES (&optional (host-id (short-site-name)))
  "Return the aliases for the host, as declared in /etc/hosts.
The argument may be a string, a symbol, or an internet address;
the default is (short-site-name)."
  (aliases (get-host host-id)))

(defun SERVER-NAME (&optional (host-id (short-site-name)))
  "Return the name of the cluster-server of the specified system,
or nil if it isn't in /etc/hosts.
The argument may be a string, a symbol, or an internet address;
the default is (short-site-name)."
  (name (get-server host-id)))

(defun SERVER-ADDRESS (&optional (host-id (short-site-name)))
  "Return the internet address of the cluster-server of the specified
system, or nil if it isn't in /etc/hosts.
The argument may be a string, a symbol, or an internet address;
the default is (short-site-name)."
  (internet-address (get-server host-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from <netdb.h>

#+:Allegro
(ff:defcstruct (hostent :malloc)
  (name * :char)			; char *h_name
  (aliases * * :char)			; char **h_aliases
  (addrtype :long)			; int h_addrtype
  (length :long)			; int h_length
  (addr * :char)			; char *h_addr   --or--
					; char **h_addr_list (for SunOS 4.0)
  )

;; from franz's ipc.cl
#+:Allegro
(ff:defcstruct unsigned-long
  (unsigned-long :unsigned-long))

#+:Allegro
(ff:defforeign-list '((gethostbyname :entry-point #,(ff:convert-to-lang
						     "gethostbyname"))
		      (gethostbyaddr :entry-point #,(ff:convert-to-lang
						     "gethostbyaddr"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#!(defun four-byte-listp (arg)
    (and (consp arg)
	 (= 4 (length arg))
	 (every #'(lambda (a) (typep a '(unsigned-byte 8)))
		arg)))

(deftype four-byte-list () '(satisfies four-byte-listp))

(defun one-longword-listp (arg)
  (and (consp arg)
       (= 1 (length arg))
       (typep (first arg) '(unsigned-byte 32))))

(deftype one-longword-list () '(satisfies one-longword-listp))

#!(defclass INTERNET-ADDRESS ()
    ((addr :initarg :addr :accessor addr :initform 0)))

(defmethod PRINT-OBJECT ((internet-address internet-address) stream)
  (format stream "#[~{~d~^.~}]" (split-internet-address internet-address)))

#!(defmethod MAKE-LOAD-FORM ((internet-address internet-address))
    (make-load-form-saving-slots internet-address))

(defun INTERNET-ADDRESS-P (object)
  "Predicate returning non-nil iff OBJECT is an internet address."
  (typep object 'internet-address))

#!(defun MAKE-INTERNET-ADDRESS (address)
    "Reader-invoked internet-address constructor.  When address is a
one-integer list, that integer becomes the 32-bit internet address.
Otherwise, address must be a four-byte list, and the bytes are
combined to make the internet address."
    (typecase address
      (four-byte-list (join-internet-address address))
      (one-longword-list (make-instance 'internet-address
			   :addr (first address)))
      (t (error "An internet-address must be either a single positive
integer, or four integers in the range [0..255]."))))

(defun SPLIT-INTERNET-ADDRESS (internet-address)
  "Return an internet address as a list of four bytes."
  (split-address (addr internet-address)))

(defun SPLIT-ADDRESS (address)
  (list (ldb (byte 8 24) address)
	(ldb (byte 8 16) address)
	(ldb (byte 8  8) address)
	(ldb (byte 8  0) address)))

#!(defun JOIN-INTERNET-ADDRESS (four-byte-list)
    "Inverse of SPLIT-INTERNET-ADDRESS."
    (let ((addr (unsigned-long-unsigned-long (make-unsigned-long))))
	 (setf (ldb (byte 8 24) addr)	(pop four-byte-list)
	       (ldb (byte 8 16) addr)	(pop four-byte-list)
	       (ldb (byte 8  8) addr)	(pop four-byte-list)
	       (ldb (byte 8  0) addr)	(car four-byte-list))
	 (make-instance 'internet-address :addr addr)))

(defclass HOST ()
  ((internet-address
	:initarg	:internet-address
	:reader		internet-address
	:initform	(make-instance 'internet-address :addr 0))
   (name
	:initarg	:name
	:reader		name
	:initform	"anonymous host")
   (aliases
	:initarg	:aliases
	:reader		aliases
	:initform	nil))
  (:documentation "Class describing internet hosts."))

(defmethod PRINT-OBJECT ((host host) stream)
  (format stream "#<~a @ ~a>" (name host) (internet-address host)))

(defmethod INTERNET-ADDRESS ((host (eql nil)))
  nil)

(defmethod NAME ((host (eql nil)))
  nil)

(defmethod ALIASES ((host (eql nil)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric GET-HOST (host)
  (:documentation
   "Return a HOST class object describing the argument.
Define a new method for handling other types of argument
to HOST-NAME and HOST-ADDRESS."))

(defmethod GET-HOST ((host STRING))
  (make-host (gethostbyname host)))

(defmethod GET-HOST ((host INTERNET-ADDRESS))
  (let ((addr (make-unsigned-long)))
    (setf (unsigned-long-unsigned-long addr) (addr host))
    (make-host (gethostbyaddr addr *number-address-bytes* *af_inet*))))

(defmethod GET-HOST ((host SYMBOL))
  ;; use the symbol's print name
  (get-host (symbol-name host)))

(defmethod GET-HOST ((host (eql nil)))
  nil)

(defun MAKE-HOST (hostent)
  "Creates and returns an object of class HOST, from the HOSTENT argument.
The C functions GETHOSTBYNAME and GETHOSTBYADDR both return a HOSTENT."
  (if (/= 0 hostent)
      (let* ((addr (hostent-addr hostent))
	     (addr2 (si:memref-int
		     ;; SunOS 4.0 requires an extra indirection
		     (if (or (member comp::.target.
				     '(:hp :sgi4d :sony :dec3100 :rs6000)
				     :test #'eq)
			     (probe-file "/lib/ld.so"))
			 (si:memref-int addr 0 0 :unsigned-long)
		       addr)
		     0 0 :unsigned-long))
	     (internet-address (make-instance 'internet-address :addr addr2))
	     (name #+:Allegro (ff:char*-to-string (hostent-name hostent))
		   #-:Allegro (break "wake up"))
	     (aliases (alias-list hostent))
	     )
	(make-instance 'host :name name :internet-address internet-address :aliases aliases))))

(defun ALIAS-LIST (hostent)
  (do* ((addr (hostent-aliases hostent) (+ 4 addr))
	(alias (si:memref-int addr 0 0 :unsigned-long) (si:memref-int addr 0 0 :unsigned-long))
	(list nil))
      ((= 0 alias) list)
    (push #+:Allegro (ff:char*-to-string alias)
	  list)))

(defun GET-SERVER (host-id)
  "Return the HOST object for host-id's server."
  (get-host (build-server-address (internet-address (get-host host-id)))))

(defun BUILD-SERVER-ADDRESS (internet-address)
  "Return the internet address of the server for the system with
the specified internet address.  This does not confirm that the
server is in /etc/hosts."
  (and internet-address
       (let ((l (split-internet-address internet-address)))
	    (if (= 1 (third l))
		internet-address
		(join-internet-address
		  (list (first l) (second l) 1 (third l)))))))

(defun DESCRIBE-HOSTENT (hostent)
  "e.g. (describe-hostent (gethostbyname \"stsci.edu\"))"
  (let ((current hostent))
    (format t "~%name:~10t~8d: ~8d = ~s" current (si:memref-int current 0 0 :unsigned-long)
	    (ff:char*-to-string (si:memref-int current 0 0 :unsigned-long)))

    (incf current 4)
    (format t "~%aliases:~10t~8d: ~8d" current (si:memref-int current 0 0 :unsigned-long))
    (do* ((addr (si:memref-int current 0 0 :unsigned-long) (+ addr 4))
	  (ptr (si:memref-int addr 0 0 :unsigned-long) (si:memref-int addr 0 0 :unsigned-long)))
	((= ptr 0)
	 (format t "~%~24t~8d: ~8d" addr ptr)
	 nil)
      (format t "~%~24t~8d: ~8d  = ~s" addr ptr (ff:char*-to-string ptr)))

    (incf current 4)
    (format t "~%addrtype:~10t~8d: ~8d" current (si:memref-int current 0 0 :unsigned-long))

    (incf current 4)
    (format t "~%length:~10t~8d: ~8d" current (si:memref-int current 0 0 :unsigned-long))

    (incf current 4)
    (format t "~%addr:~10t~8d: ~8d" current (si:memref-int current 0 0 :unsigned-long))
    (do* ((addr (si:memref-int current 0 0 :unsigned-long) (+ addr 4))
	  (ptr (si:memref-int addr 0 0 :unsigned-long) (si:memref-int addr 0 0 :unsigned-long)))
	((= ptr 0)
	 (format t "~%~24t~8d: ~8d" addr ptr)
	 nil)
      (format t "~%~24t~8d: ~8d  = ~s" addr ptr 
	      (split-address (si:memref-int ptr 0 0 :unsigned-long))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros.

#!(defun |#[-reader| (stream subchar arg)
    "Reader function for the dispatch macro #[.
#[foo] is equivalent to (make-internet-address foo))."
    (declare (ignore subchar arg))
    (let ((*readtable* (copy-readtable)))
	 (set-syntax-from-char #\. #\space)
	 (make-internet-address (read-delimited-list #\] stream t))))

#!(set-dispatch-macro-character #\# #\[ #'|#[-reader|)
#!(set-macro-character #\] (get-macro-character #\)) nil)	;CLtL2, pp 572
#!(tpl:setq-default *readtable* *readtable*)			; not a NOP!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *HOSTNAME-DEFAULTS* ".stsci.edu"
  ;; rewrite this illiterate comment
  "A string used by the HOSTCASE macro to expand hostnames.  Each part of the
string is treated separately, starting from the end, working inwards until a
part of *HOSTNAME-DEFAULTS* matches the host string specified. Case is
ignored.
E.g., if *hostname-defaults* is \"stsci.edu\", the following are
some expansions:
   kali        -->  kali.stsci.edu
   kali.stsci  -->  kali.stsci.edu
   foo.edu     -->  foo.edu
   foo         -->  foo.stsci.edu
   foo.ed      -->  foo.ed.stsci.edu  "
  )

(defmacro HOSTCASE (keyform &rest clauses)
  "Just like case, but works with hostnames.  KEYFORM must evaluate to a string
or a symbol, that is expanded by EXPAND-HOSTNAME using the defaults in
*HOSTNAME-DEFAULTS*.  Each element of CLAUSES is a list of
   (key consequent-1 consequent-2 ...)
KEY is a hostname; it is not evaluated but is expanded as described
above; it must be a string, an unquoted symbol, or a list of the same.
The symbols T and OTHERWISE are treated specially, as by CASE.

Example:
   ;;; *hostname-defaults* = \".stsci.edu\"
   (hostcase (short-site-name)
     (\"axolotl.stsci.edu\" \"Thank you for flying Axolotl air\")
     (kali \"The Goddess welcomes you.\")
     ((prufrock.stsci.edu marian) \"You have chosen prufrock or marian\")
     (t \"no preference, eh?\"))"

  `#!(case (host-symbol ,keyform)
       ,@(mapcar #'process-hostcase-clause clauses)))

(defmacro EHOSTCASE (keyform &rest clauses)
  "Like HOSTCASE, but error if no case matches"
  `#!(ecase (host-symbol ,keyform)
       ,@(mapcar #'process-hostcase-clause clauses)))

;;; this chostcase isn't very nice, but may be better than nothing.

(defmacro CHOSTCASE (keyform &rest clauses)
  (let ((temp (gensym)))
    `#!(let ((,temp (host-symbol ,keyform)))
	 (ccase ,temp
	   ,@(mapcar #'process-hostcase-clause clauses)))))


(defun host-symbol (hostname)
  "Return HOSTNAME as an uppercase symbol.  The name is expanded
with the defaults from *HOSTNAME-DEFAULTS, as by EXPAND-HOSTNAME."
  (let ((string-hostname (if (symbolp hostname)
			     (symbol-name hostname)
			   hostname)))
    (values (intern (string-upcase (expand-hostname string-hostname))))))

(defun expand-hostname (string &optional (hostname-defaults *hostname-defaults*))
  "Expand hostnames, as described in the *HOSTNAME-DEFAULTS* doc string."
  (flet ((tail-match-p (seq1 seq2)
	   (let ((mismatch (mismatch seq1 seq2 :from-end t :test #'char-equal)))
	     (or (null mismatch)
		 (= 0 mismatch))))
	 (canonical-hostname-defaults (hostname-defaults)
	   "Return hostname-defaults as a string with a leading \".\""
	   (cond ((null hostname-defaults)
		  "")
		 ((equal "" hostname-defaults)
		  "")
		 ((char= #\. (elt hostname-defaults 0))
		  hostname-defaults)
		 (t
		  (concatenate 'string "." hostname-defaults))))
	 )
    (let ((defaults (canonical-hostname-defaults hostname-defaults)))
      (do* ((end (length defaults) start)
	    (start (or (position #\. defaults :from-end t) 0)
		   (or (position #\. defaults :from-end t :end end) 0))
	    (rest "" (concatenate 'string tail rest))
	    (tail (subseq defaults start)
		  (subseq defaults start end)))
	  ((tail-match-p tail string)
	   (concatenate 'string string rest))))))

(defun process-hostcase-clause (clause)
  (assert (listp clause))
  (let ((key (car clause))
	(consequents (cdr clause)))
    (typecase key
      (string (cons (host-symbol key)
		    consequents))
      (list (cons (mapcar #'host-symbol key)
		  consequents))
      (symbol (if (or (eq key t)
		      (eq key 'otherwise))
		  clause
		(cons (host-symbol key)
		      consequents))))))

(define-test-function host-address
  :key #'addr
  ((host-address "kali")		#[130.167.104.14])
  ((host-address "eyeof")		#[130.167.1.24])
  ((host-address #[130.167.104.14])	#[130.167.104.14])
  ((host-address)			(host-address (short-site-name)))
)

#|
(define-test-function host-aliases
  :key #'(lambda (set) (sort (copy-list set) #'string-lessp))
  ((host-aliases "kali.stsci.edu")	'("kali"))
  ((host-aliases "dmfdev.stsci.edu")	'("dmfdev" "dmfdev.stsci.edu"
					  "eyeof" "eyeof.stsci.edu"))
  ((host-aliases)			(host-aliases (short-site-name)))
)
|#

#| Jul 28 1993 Mike Rose
(define-test-function host-name
  :test #'equalp
  ((host-name "kali")			"kali.stsci.edu")
  ((host-name #[130.167.1.107])		"marian.stsci.edu")
  ((host-name)				(host-name (short-site-name)))
)

(define-test-function host-utils-null
  ((host-address nil)			nil)
  ((host-address "foobar")		nil)
  ((host-address #[0.0.0.0])		nil)
  ((host-aliases nil)			nil)
  ((host-aliases "foo")			nil)
  ((host-aliases #[0.0.0.0])		nil)
  ((host-name nil)			nil)
  ((host-name "foobar")			nil)
  ((host-name #[0.0.0.0])		nil)
  ((server-address nil)			nil)
  ((server-address "foo")		nil)
  ((server-address #[0.0.0.0])		nil)
  ((server-name nil)			nil)
  ((server-name "foo")			nil)
  ((server-name #[0.0.0.0])		nil)
)

(define-test-function server-address
  :key #'addr
  ((server-address "kali")		(host-address "cerberus"))
  ((server-address "marian")		(host-address "marian"))
)

(define-test-function server-name
  :test #'equalp
  ((server-name "kali")			"cerberus.stsci.edu")
  ((server-name "marian")		"marian.stsci.edu")
  ((server-name (host-address "kali"))	"cerberus.stsci.edu")
  ((server-name)			(server-name (short-site-name)))
)

(define-test-function hostcase1
  :let ((*hostname-defaults* ".stsci.edu"))
  ((hostcase "kali"
	(foo			nil)
	(marian			nil)
	(kali			t))	t)
  ((hostcase "kali"
	(foo			nil)
	(t			t))	t)
  ((hostcase "kali.stsci"
	(kali.edu		nil)
	(kali.stsci		t)
	(t			nil))	t)
)

(define-test-function hostcase2
  :let ((*hostname-defaults* ".a.b.stsci.edu"))
  ((hostcase "kali"
	(kali.stsci		nil)
	(kali.a.b		t))	t)
  ((hostcase "kali.b"
	(kali			nil)
	(kali.b.stsci.edu	t))	t)
  ((hostcase "kali.stsci"
	(kali.stsci.edu		t))	t)
)

|#
