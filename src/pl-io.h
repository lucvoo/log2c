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
struct stream *PL_Output_Stream(union cell *s);
struct stream *PL_Input_Stream(union cell *s);
struct stream *PL_OutStream(void);
struct stream *PL_InStream(void);

#endif
