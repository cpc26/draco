/*
	print a single GEIS header file keyword value.
	use geis_p to insure that the file is a GEIS header file.

	syntax:	GEIS-kval keyword_name filename
*/

#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <sysexits.h>

#include <FITS-lex.h>


#define Program_name	*argv
#define Keyword_name	argv[1]
#define Input_filename	argv[2]


Token *
Token_get(stream)
    FILE *		stream;
{
    static char		buf[CARD_LENGTH + 2];	/* +2 for '\n' and '\0'	*/
    register Token *	token = NULL;

    if (fgets(buf, sizeof(buf), stream)) {
	buf[CARD_LENGTH] = '\0';		/* clobber '\n'	*/
	token = Token_create();
	strncpy(token->keyword, buf, KEYWORD_LENGTH);
	strcpy(token->value, &buf[KEYWORD_LENGTH]);
    }

    return token;
}


int
main(argc, argv)
    int			argc;
    char *		argv[];
{
    register FILE *	stream;
    register Token *	token;
    register int	keyword_length;

    if (argc != 3) {
	fprintf(stderr, "usage: %s keyword filename\n", Program_name);
	return EX_USAGE;
    }

    if (geis_p(Input_filename) != EX_OK) {
	fprintf(stderr, "%s: %s is not a valid GEIS header file.\n",
		Program_name, Input_filename);
	return EX_DATAERR;
    }

    keyword_length = strlen(Keyword_name);
    stream = fopen(Input_filename, "r");

    while (token = Token_get(stream)) {
	if (!strncasecmp(Keyword_name, token->keyword, keyword_length)) {
	    printf("%s\n", isolate_fits_value(token->value));
	    free((char *) token);
	    break;
	}
	free((char *) token);
    }

    fclose(stream);
    return EX_OK;
}
