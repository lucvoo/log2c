/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_IO_H_
#define	PL_IO_H_


#include "Prolog.h"

/* pl-io.c */
pl_stream PL_Output_Stream(term_t s);
pl_stream PL_Input_Stream(term_t s);
pl_stream PL_OutStream(void);
pl_stream PL_InStream(void);

#endif
