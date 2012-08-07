/*
	FITS (and GEIS) keyword recognizer.  a legal keyword is "a left
	justified, 8-character, blank filled, ASCII string with no
	embedded blanks" (cf. FITS Implementation Standard).  the legal
	characters are [0-9], [A-Z], hyphen, and underscore.

	(i've added "/" because the keyword "IRAF-B/P" appears in our
	 RSG data. -- fwy)
*/

#include <string.h>

#include <FITS-p.h>

#define FALSE	0
#define TRUE	1


int
begins_with_FITS_keyword(bp)
    char *		bp;
{
    static char		kwd_chars[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-_/";
    register char *	cp;
    register int	len;

    if ((len = strspn(bp, kwd_chars)) > KEYWORD_LENGTH)
	return FALSE;

    for (cp = bp + KEYWORD_LENGTH; cp-- > bp + len;)
	if (*cp != ' ') return FALSE;

    return TRUE;
}
