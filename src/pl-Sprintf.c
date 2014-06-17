/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include <stdarg.h>
#include <stdlib.h>			// For free(3)
#include <stdio.h>			// For vasprintf(3)

#include "pl-stream.h"

static int Svprintf(struct stream *S, const char *fmt, va_list ap)
{
	int rval;
	char *buf;

	if ((rval = vasprintf(&buf, fmt, ap)) < 0)
		return (rval);

	rval = Sputs(S, buf);
	free(buf);
	return (rval);
}

int Sprintf(const char *fmt, ...)
{
	int rval;
	va_list ap;

	va_start(ap, fmt);
	rval = Svprintf(Stdout, fmt, ap);
	va_end(ap);
	return (rval);
}

int Sprintf_err(const char *fmt, ...)
{
	int rval;
	va_list ap;

	va_start(ap, fmt);
	rval = Svprintf(Stderr, fmt, ap);
	va_end(ap);
	return (rval);
}

int Sfprintf(struct stream *S, const char *fmt, ...)
{
	int rval;
	va_list ap;

	va_start(ap, fmt);
	rval = Svprintf(S, fmt, ap);
	va_end(ap);
	return (rval);
}
