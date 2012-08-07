/*
	FITS header file lexical analyzer.  use fits_p
	to insure that the file is a FITS header file.

	syntax:	FITS-lex [ -k ] filename
*/

#include <ctype.h>
#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <sysexits.h>

#include <FITS-lex.h>


extern void exit();


Token *
Token_create()
{
    register Token *	token;

    token = (Token *) malloc(sizeof(Token));
    if (token == NULL) {
	fprintf(stderr, "Token_create: malloc failure (EX_SOFTWARE).\n");
	exit(EX_SOFTWARE);
    }

    return token;
}


static char *
find_first_value_char(value_string)
    char *		value_string;
{
    register char *	cp;

    for (cp = value_string;
	 *cp == ' ' || *cp == '\t' || *cp == '=';
	 cp++)
	;
    return cp;
}


static char *
find_last_value_char(value_string_ptr)
    char *		value_string_ptr;
{
    register char *	cp;

    for (cp = value_string_ptr; isspace(*cp); cp--)
	;
    return cp;
}


static char *
find_next_char(value_string_ptr, search_char)
    char *		value_string_ptr;
    char		search_char;
{
    register char *	cp;

    for (cp = value_string_ptr; *cp && *cp != search_char; cp++)
	;
    return cp;
}


/*
	this is particularly crude.  first, it overwrites the input
	string (which happens to be malloc'ed memory!).  second, it
	doesn't know anything about FITS semantics, so it may screw
	up badly on COMMENT keyword values, for example.  still, it
	should work for early versions of Draco report generators.
*/

char *
isolate_fits_value(value_string)
    char *		value_string;
{
    register char *	cp;
    register char *	first;

    cp = first = find_first_value_char(value_string);
    switch (*cp) {
	case '/':	/*  nasty blank keyword comment case	*/
	case '\0':	/*  very nasty blank line comment case	*/
	    first = value_string;
	    break;
	    
	case '\'':
	    cp = find_next_char(++cp, '\'');
	    *++cp = '\0';
	    break;

	default:
	    cp = find_next_char(++cp, '/');
	    cp = find_last_value_char(--cp);
	    *++cp = '\0';
	    break;
    }
    return first;
}


#ifdef FITS_LEX_MAIN
#define FALSE		0
#define TRUE		1

#define Input_filename	argv[argc-1]


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


static int
fits_lex_usage(stream, image_name)
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
    register int	ncards = RECORD_LENGTH / CARD_LENGTH;
    register int	print_keyword_value = TRUE;

    if (argc == 1)	fits_lex_usage(stderr, *argv);

    if (!strcmp(argv[1], "-k")) {
	if (argc == 2)	fits_lex_usage(stderr, *argv);
	print_keyword_value = FALSE;
    }

    if (fits_p(Input_filename) != EX_OK) {
	fprintf(stderr, "%s: %s is not a valid FITS header file.\n",
		*argv, Input_filename);
	return EX_DATAERR;
    }

    stream = fopen(Input_filename, "r");

    while (ncards-- > 0) {
	token = Token_get(stream);
	printf("%8.8s\n", token->keyword);
	if (print_keyword_value)
	    printf("%s\n", isolate_fits_value(token->value));
	free((char *) token);
    }

    fclose(stream);
    return EX_OK;
}
#endif	/* FITS_LEX_MAIN */
