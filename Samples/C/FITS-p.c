/*
	halfhearted FITS file recognizer.  checks for the following FITS
	file attributes (cf. ICD 19):

	    1)	the file's size must be a multiple of RECORD_LENGTH.
	    2)	the bytes in the primary header (first RECORD_LENGTH
		bytes) must be between 0x20 and 0x7e inclusive.
	    3)	the 80-byte "card images" in the primary header must
		each begin with a legal keyword, "a left justified,
		8-character, blank filled, ASCII string with no
		embedded blanks" (cf. FITS Implementation Standard).
		the legal characters are [0-9], [A-Z], hyphen, and
		underscore.  (is "/" legal??)
	    4)	the first "card image" must be "SIMPLE = T".

	to create test driver:	#define FITS_P_MAIN.

	test driver syntax:	FITS-p  filename
*/

#include <stdio.h>
#include <string.h>
#include <sysexits.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <FITS-p.h>

extern int begins_with_FITS_keyword(/*  char *card_image  */);


int
fits_p(filename)
    char *		filename;
{
    static char		buf[RECORD_LENGTH];
    static struct stat	ss;
    register FILE *	stream;
    register char *	bp;

    if ((stream = fopen(filename, "r")) == NULL)
	return EX_NOINPUT;			/*  do not pass go ...	*/

    if (stat(filename, &ss) || ss.st_size % RECORD_LENGTH)
	return EX_DATAERR;			/*  do not collect $200	*/

    if (RECORD_LENGTH != fread(buf, sizeof(char), RECORD_LENGTH, stream))
	return EX_DATAERR;

    for (bp = &buf[RECORD_LENGTH]; bp-- > buf;)
	if (*bp < 0x20 || 0x7e < *bp) return EX_DATAERR;

    for (bp = buf; bp < &buf[RECORD_LENGTH]; bp += CARD_LENGTH)
	if (!begins_with_FITS_keyword(bp)) return EX_DATAERR;

    if (strncmp(buf, "SIMPLE  = ", 10)) return EX_DATAERR;
    for (bp = &buf[10]; *bp == ' '; bp++)	/*  skip whitespace	*/
	;
    if (*bp != 'T') return EX_DATAERR;

    return EX_OK;
}


#ifdef FITS_P_MAIN
int
main(argc, argv)
    int			argc;
    char *		argv[];
{
    register char *	filename;
    register int	status;
    register int	verbose = 0;	/* there used to be a -v option	*/

    switch (argc) {
	case 2:
	    filename = argv[1];

	    switch (status = fits_p(filename)) {
		case EX_DATAERR:
		    if (verbose)
			fprintf(stderr,
				"%s: %s is not a FITS file (EX_DATAERR).\n",
				*argv, filename);
		    break;

		case EX_NOINPUT:
		    if (verbose)
			fprintf(stderr,
				"%s: %s cannot be read (EX_NOINPUT)\n",
				*argv, filename);
		    break;

		case EX_OK:
		    fprintf(stdout, "FITS file (EX_OK)\n");
		    break;

		default:
		    if (verbose)
			fprintf(stderr,
				"%s: unexpected return status (%d)\n",
				*argv, status);
		    break;
	    }
	    break;

	default:
	    status = EX_USAGE;
	    if (verbose)
		fprintf(stderr, "usage: %s <filename>\n", *argv);
	    break;
    }

    return status;
}
#endif	/* FITS_P_MAIN */
