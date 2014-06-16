/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef	PL_IO_H_
#define	PL_IO_H_


#include "Prolog.h"

/* pl-io.c */
pl_stream PL_Output_Stream(term_t s);
pl_stream PL_Input_Stream(term_t s);
pl_stream PL_OutStream(void);
pl_stream PL_InStream(void);

#endif
