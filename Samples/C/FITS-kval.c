/*
	print a single FITS header file keyword value.
	use fits_p to insure that the file is a FITS header file.

	syntax:	FITS-kval keyword_name filename
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
    static char		buf[CARD_LENGTH];
    register Token *	token = NULL;

    if (fread(buf, sizeof(char), CARD_LENGTH, stream)) {
	token = Token_create();
	strncpy(token->keyword, buf, KEYWORD_LENGTH);
	strncpy(token->value, &buf[KEYWORD_LENGTH],
		CARD_LENGTH - KEYWORD_LENGTH);
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
    register int	ncards = RECORD_LENGTH / CARD_LENGTH;

    if (argc != 3) {
	fprintf(stderr, "usage: %s keyword filename\n", Program_name);
	return EX_USAGE;
    }

    if (fits_p(Input_filename) != EX_OK) {
	fprintf(stderr, "%s: %s is not a valid FITS header file.\n",
		Program_name, Input_filename);
	return EX_DATAERR;
    }

    keyword_length = strlen(Keyword_name);
    stream = fopen(Input_filename, "r");

    while (ncards-- > 0) {
	token = Token_get(stream);
	if (!strncasecmp(Keyword_name, token->keyword, keyword_length)) {
	    printf("%s\n", isolate_fits_value(token->value));
	    ncards = 0;
	}
	free((char *) token);
    }

    fclose(stream);
    return EX_OK;
}
