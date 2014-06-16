/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef	PL_WRITE_H_
#define	PL_WRITE_H_

#include "Prolog.h"
#include "pl-stream.h"

/* pl-write.c */
int PL_display(pl_stream S, term_t t);
int PL_displayq(pl_stream S, term_t t);
int PL_write(pl_stream S, term_t t);
int PL_writeq(pl_stream S, term_t t);
int PL_puts(char *s);
int pl_warn(const char *fmt);

#endif
