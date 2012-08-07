/*
	GEIS header file lexical analyzer.  use geis_p
	to insure that the file is a GEIS header file.

	syntax:	GEIS-lex [ -k ] filename
*/

#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <sysexits.h>

#include <FITS-lex.h>

#define FALSE		0
#define TRUE		1

#define Input_filename	argv[argc-1]


extern void exit();


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


static int
geis_lex_usage(stream, image_name)
    FILE *		stream;
    char *		image_name;
{
    fprintf(stream, "usage: %s [ -k ] filename\n", image_name);
    fprintf(stream, "       -k:  display keywords only\n");
    exit(EX_USAGE);
}


int
main(argc, argv)
    int			argc;
    char *		argv[];
{
    register FILE *	stream;
    register Token *	token;
    register int	print_keyword_value = TRUE;

    if (argc == 1)	geis_lex_usage(stderr, *argv);

    if (!strcmp(argv[1], "-k")) {
	if (argc == 2)	geis_lex_usage(stderr, *argv);
	print_keyword_value = FALSE;
    }

    if (geis_p(Input_filename) != EX_OK) {
	fprintf(stderr, "%s: %s is not a valid GEIS header file.\n",
		*argv, Input_filename);
	return EX_DATAERR;
    }

    stream = fopen(Input_filename, "r");

    while (token = Token_get(stream)) {
	printf("%8.8s\n", token->keyword);
	if (print_keyword_value)
	    printf("%s\n", isolate_fits_value(token->value));
	free((char *) token);
    }

    fclose(stream);
    return EX_OK;
}
