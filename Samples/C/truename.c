/*
	truename (path, buf, bufsiz)

	path is copied to buf if it is a file that is not a symbolic link.
	if it is a symbolic link, the link is followed until an actual file
	is found and this file's name is copied to buf.

	returns 0 if a filename was written into buf; returns errno otherwise.

	to create test driver:	#define TRUENAME_MAIN.

	test driver syntax:	truename  filename
*/

#include <errno.h>
#include <string.h>


int
truename (path, buf, bufsiz)
	char *		path;		/* link name	*/
	char *		buf;		/* filename buf	*/
	int		bufsiz;		/* sizeof(buf)	*/
{
	register int	status;

	switch (readlink(path, buf, bufsiz)) {
	    case -1:
		switch (errno) {
		    case EINVAL:
			strncpy(buf, path, bufsiz);
			buf[--bufsiz] = '\0';
			status = 0;
			break;

		    default:
			status = errno;
			break;
		}
		break;

	    default:
		status = truename(buf, buf, bufsiz);
		break;
	}

	return status;
}


#ifdef TRUENAME_MAIN
#include <stdio.h>
#include <sysexits.h>

static char		buf[BUFSIZ];

int
main (argc, argv)
	int		argc;
	char *		argv[];
{
	register int	status;

	switch (argc) {
	    case 2:
		switch (truename(argv[1], buf, sizeof(buf))) {
		    case 0:
			fputs(buf, stdout);
			putchar('\n');
			status = EX_OK;
			break;

		    default:
			fprintf(stderr, "%s: errno = %d for %s\n",
				*argv, errno, argv[1]);
			status = EX_NOINPUT;
			break;
		}
		break;

	    default:
		fprintf(stderr, "usage: %s <filename>\n", *argv);
		status = EX_USAGE;
		break;
	}

	return status;
}
#endif	/* TRUENAME_MAIN */
