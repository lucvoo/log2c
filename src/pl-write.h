/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_WRITE_H_
#define	PL_WRITE_H_

#include "Prolog.h"
#include "pl-stream.h"

/* pl-write.c */
int PL_display(term_t t, pl_stream S);
int PL_displayq(term_t t, pl_stream S);
int PL_write(term_t t, pl_stream S);
int PL_writeq(term_t t, pl_stream S);
int PL_puts(char *s);
int pl_warn(const char *fmt);

#endif
