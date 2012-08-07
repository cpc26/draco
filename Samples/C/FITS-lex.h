/*
	FITS header file lexical analyzer interface.
	Use fits_p to insure that the file actually
	is a FITS header file.
*/

#ifndef fits_lex_h_included
#define fits_lex_h_included

#include <FITS-p.h>

typedef char Token_keyword[KEYWORD_LENGTH + 1];
typedef char Token_value[CARD_LENGTH - KEYWORD_LENGTH + 1];
typedef struct _Token {Token_keyword keyword; Token_value value} Token;

extern Token *	Token_create(/*  void  */);
extern char *	isolate_fits_value(/*  char *value_string  */);

#endif	/* fits_lex_h_included */
