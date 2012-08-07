c PHEAD -- Print the header of an OIF image in Draco lexical analyzer
c output format (a stream of keyword and value tokens, each token being
c an ASCII string, possibly containing blanks, terminated by a newline
c character).
c
c Adapted from the Imfort sample program of the same name.
c
c	usage:  phead image_name
c ----------------------------------------------------------------------------

	program phead

	character*80	image
	character*20	kwname
	character*8	kwords(30)
	integer		ier, im, kwl

	data kwords /	'object  ', 'date    ', 'iraf-max', 'iraf-min',
     *			'iraf-b/p', 'observat', 'ccdpicno', 'exptime ',
     *			'darktime', 'otime   ', 'imagetyp', 'date-obs',
     *			'ra      ', 'dec     ', 'epoch   ', 'zd      ',
     *			'ha      ', 'ut      ', 'st      ', 'airmass ',
     *			'detector', 'camtemp ', 'dewtemp ', 'filters ',
     *			'trimsec ', 'datasec ', 'biassec ', 'ccdsum  ',
     *			'vebgain ', 'secppix '/

c --- Get image name.
	call clargc (1, image, ier)
	if (ier .ne. 0) goto 99

c --- Open the image.
	call imopen (image, 1, im, ier)
	if (ier .ne. 0) goto 99

c --- Print the standard keyword-value pairs on the standard output device.
	call imokwl (im, '*', .false., kwl, ier)
	if (ier .ne. 0) goto 98

 10	continue
	call imgnkw (kwl, kwname, ier)
	if (ier .ne. 0) then
	    call putkey (im, kwname, ier)
	    if (ier .ne. 0) goto 20
	    goto 10
	endif

 20	continue
	call imckwl (kwl, ier)

c --- Print the remaining keyword-value pairs on the standard output device.
c --- (This kludge is necessitated by an IMFORT bug.  In theory, the previous
c --- loop would have gotten us all the keyword-value pairs.  This loop should
c --- be removed when the bug is fixed.)
	do 30 i = 1, 30
	    call imokwl(im, kwords(i), .false., kwl, ier)
	    if (ier .ne. 0) goto 98
	    call imgnkw (kwl, kwname, ier)
	    if (ier .eq. 0) call putkey (im, kwname, ier)
	    call imckwl (kwl, ier)
 30	continue

c --- Clean up.
 98	continue
	call imclos (im, ier)
 99	continue
	stop
	end


c PUTKEY -- Read the value and comment fields of the named image header
c keyword, and print the value of the keyword in Draco lexical analyzer
c output format (a stream of keyword and value tokens, each token being
c an ASCII string, possibly containing blanks, terminated by a newline
c character) on the standard output device.
c
c Datatype codes: 1=bool, 2=char, 3,4,5=int, 6,7=real/double, 8=complex
c Only codes 1, 2, 4, and 6 (bool,char,int,real) are returned by IMTYPK.
c ------------------------------------------------------------------------

	subroutine putkey (im, kwname, ier)

	integer		im
	character*(*)	kwname

	logical		bval
	character*68	sval
	integer		ival
	doubleprecision dval

	character*47	comment
	character*70	value
	integer		nchars, dtype, ier, i

c --- Get the keyword data type and comment information.
	call imtypk (im, kwname, dtype, comment, ier)
	if (ier .ne. 0) goto 99

c --- Get the keyword value.
c -----	bool value
	if (dtype .eq. 1) then
	    call imgkwb (im, kwname, bval, ier)
	    if (ier .ne. 0) goto 99
	    write (value, 10) bval
 10	    format (1x, l20)

c -----	char value
	else if (dtype .eq. 2) then
	    call imgkwc (im, kwname, sval, ier)
	    if (ier .ne. 0) goto 99

c --------- find last non-blank char
	    nchars = len(sval) - 1
	    do 20 i = nchars, 9, -1
		if (sval(i:i) .ne. ' ') goto 30
		nchars = i - 1
 20	    continue
 30	    continue

c --------- copy sval into value adding delimiting pops
	    nchars = min (nchars, len(value) - 2)
	    value(1:1) = ''''
	    do 40 i = 1, nchars
		value(i+1:i+1) = sval(i:i)
 40	    continue
	    value(nchars+2:nchars+2) = ''''
	    do 50 i = nchars + 3, len(value)
		value(i:i) = ' '
 50	    continue

c -----	int value
	else if (dtype .ge. 3 .and. dtype .le. 5) then
	    call imgkwi (im, kwname, ival, ier)
	    if (ier .ne. 0) goto 99
	    write (value, 60) ival
 60	    format (1x, i20)

c -----	real/double value
	else if (dtype .ge. 6 .and. dtype .le. 7) then
	    call imgkwd (im, kwname, dval, ier)
	    if (ier .ne. 0) goto 99
	    if (abs(dval) .lt. 1.0E6 .and. abs(dval) .ge. 1.0E-1) then
                write (value, 70) dval
 70		format (1x, f20.2)
            else
                write (value, 80) dval
 80		format (1x, e20.12)
            endif

c -----	impossible value
	else
	    ier = -1
	    goto 99
	endif

c --- Print the keyword and value tokens.
	call prntlft (kwname, .true.)
	call prntlft (value, .false.)
	ier = 0

 99	continue
	end


c PRNTLFT -- Print the argument string ignoring any leading blanks
c (probably not portable).  if ucase is .true., the string is first
c converted to upper case.
c ------------------------------------------------------------------------

	subroutine prntlft (chars, ucase)

	character*(*)	chars
	logical		ucase

	integer		end, i, start

	do 10 start = 1, len(chars)
	    if (chars(start:start) .ne. ' ') goto 20
 10	continue
 20	continue

	do 30 end = len(chars), start, -1
	    if (chars(end:end) .ne. ' ') goto 40
 30	continue
 40	continue

	if (ucase) then
	    do 50 i = start, end
		if ('a' .le. chars(i:i) .and. chars(i:i) .le. 'z') then
		    chars(i:i) = char (ichar (chars(i:i)) - 32)
		endif
 50	    continue
	endif

	print *, chars(start:end)
	return
	end
