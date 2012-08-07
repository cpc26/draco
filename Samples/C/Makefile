PROGS = FITS-p GEIS-p FITS-lex GEIS-lex FITS-kval GEIS-kval truename
INCLUDE = -I. -I/usr/include
TOOLDIR = /axolotl/data1/Draco/Tools

all:		$(PROGS)

lint:		FITS-p.c GEIS-p.c GEIS-lex.c keyword-p.c
	lint $(INCLUDE) -DFITS_P_MAIN FITS-p.c keyword-p.c
	lint $(INCLUDE) -DGEIS_P_MAIN GEIS-p.c keyword-p.c
	lint $(INCLUDE) -DFITS_LEX_MAIN FITS-lex.c FITS-p.c keyword-p.c
	lint $(INCLUDE) GEIS-lex.c FITS-lex.c GEIS-p.c keyword-p.c
	lint $(INCLUDE) FITS-kval.c FITS-lex.c FITS-p.c keyword-p.c
	lint $(INCLUDE) GEIS-kval.c FITS-lex.c GEIS-p.c keyword-p.c
	lint $(INCLUDE) -DTRUENAME_MAIN truename.c

FITS-p:		FITS-p.c keyword-p.c
	cc $(INCLUDE) -O -o $@ -DFITS_P_MAIN FITS-p.c keyword-p.c

GEIS-p:		GEIS-p.c keyword-p.c
	cc $(INCLUDE) -O -o $@ -DGEIS_P_MAIN GEIS-p.c keyword-p.c

FITS-lex:	FITS-lex.c FITS-p.c keyword-p.c
	cc $(INCLUDE) -O -o $@ -DFITS_LEX_MAIN FITS-lex.c FITS-p.c keyword-p.c

GEIS-lex:	FITS-lex.c GEIS-lex.c GEIS-p.c keyword-p.c
	cc $(INCLUDE) -O -o $@ FITS-lex.c GEIS-lex.c GEIS-p.c keyword-p.c

FITS-kval:	FITS-kval.c FITS-lex.c FITS-p.c
	cc $(INCLUDE) -O -o $@ FITS-kval.c FITS-lex.c FITS-p.c keyword-p.c

GEIS-kval:	GEIS-kval.c GEIS-lex.c GEIS-p.c
	cc $(INCLUDE) -O -o $@ GEIS-kval.c FITS-lex.c GEIS-p.c keyword-p.c

truename:	truename.c
	cc $(INCLUDE) -O -o $@ -DTRUENAME_MAIN truename.c

install:	all
	mv -f $(PROGS) $(TOOLDIR)

clean:
	rm -f $(PROGS) *.o


FITS-lex.c:	FITS-lex.h

GEIS-lex.c:	FITS-lex.h

FITS-p.c:	FITS-p.h

GEIS-p.c:	FITS-p.h

FITS-kval.c:	FITS-lex.h

GEIS-kval.c:	FITS-lex.h

FITS-lex.h:	FITS-p.h
