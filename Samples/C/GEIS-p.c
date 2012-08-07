/*
	GEIS header file recognizer.  a GEIS header file obeys the following
	rules (cf. ICD-19).

	    1)	it must have a three-character extension.
	    2)	the third character of the extension must be "h".
	    3)	it must only contain 80-byte records terminated
		by newline characters.
	    4)	it must contain at least one record.
	    5)	each record must begin with a legal keyword, "a left
		justified, 8-character, blank filled, ASCII string
		with no embedded blanks."  (FITS Implementation Standard)
		legal characters are [0-9], [A-Z], "_", and "-".
	    6)	all remaining record bytes *should* be between 0x20
		and 0x7e inclusive, but we aren't performing this
		check because the tab character (0x09) is sometimes
		used.  (see CHECK_BYTE_VALIDITY.)

	to create test driver:	#define GEIS_P_MAIN.

	test driver syntax:	GEIS-p  filename
*/

#include <stdio.h>
#include <string.h>
#include <sysexits.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <FITS-p.h>


int
geis_p(filename)
    char *		filename;
{
    static char buf[CARD_LENGTH + 1];		/*  +1 for null term.	*/
    static struct stat	ss;
    register FILE *	stream;
    register char *	cp;

    cp = strrchr(filename, '\0');

    if (*--cp != 'h'	||			/*  no trailing 'h'	*/
	*--cp == '.'	||			/*  one char. extension	*/
	*--cp == '.'	||			/*  two char. extension	*/
	*--cp != '.')				/*  extension too long	*/
	return EX_DATAERR;			/*  do not pass go ...	*/

    if ((stream = fopen(filename, "r")) == NULL)
	return EX_NOINPUT;			/*  do not pass go ...	*/

    if (stat(filename, &ss) || !ss.st_size || ss.st_size % (CARD_LENGTH + 1))
	return EX_DATAERR;

    while (fgets(buf, sizeof(buf), stream)) {
#ifdef CHECK_BYTE_VALIDITY
	for (cp = buf; *cp > 0x1F && *cp < 0x7F; cp++)
	    ;
#else
	cp = &buf[strlen(buf)];
#endif
	if (*cp		||			/*  invalid byte	*/
	    cp - buf < CARD_LENGTH ||		/*  record too short	*/
	    getc(stream) != (int) '\n' ||	/*  no '\n' terminator	*/
	    !begins_with_FITS_keyword(buf)) {	/*  illegal keyword	*/
	    fclose(stream);
	    return EX_DATAERR;
	}
    }

    fclose(stream);
    return EX_OK;
}


#ifdef GEIS_P_MAIN
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

	    switch (status = geis_p(filename)) {
		case EX_DATAERR:
		    if (verbose)
			fprintf(stderr,
				"%s: %s is not a GEIS header file (EX_DATAERR).\n",
				*argv, filename);
		    break;

		case EX_NOINPUT:
		    if (verbose)
			fprintf(stderr,
				"%s: %s cannot be read (EX_NOINPUT)\n",
				*argv, filename);
		    break;

		case EX_OK:
		    fprintf(stdout, "GEIS header file (EX_OK)\n");
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
#endif	/* GEIS_P_MAIN */
