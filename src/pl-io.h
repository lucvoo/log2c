/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_IO_H_
#define	PL_IO_H_


#include "Prolog.h"

/* pl-io.c */
void init_io(void);
void exit_io(void);
pl_stream Output_Stream(term_t s);
pl_stream Input_Stream(term_t s);
pl_stream OutStream(void);
pl_stream InStream(void);

#endif
