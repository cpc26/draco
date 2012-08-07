/*
	FITS header file recognizer interface.
*/

#ifndef fits_p_h_included
#define fits_p_h_included

#define CARD_LENGTH	80
#define KEYWORD_LENGTH	8
#define RECORD_LENGTH	2880

extern int fits_p(/*  char *filename  */);
extern int legal_keyword(/*  char *card_image  */);

#endif	/* fits_p_h_included */
