/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_STREAM_IMPL_H_
#define	PL_STREAM_IMPL_H_

#include "pl-stream.h"

#ifdef	HAVE_PIPE
#include <unistd.h>		// for pid_t
#endif


typedef int	fd_t;

struct pl_stream_t { fd_t	fd;
#ifdef	HAVE_PIPE
		     pid_t	pid;		// for pipe stream
#endif
		     Smode_t	mode;
		     Stype_t	type;
		     Sbuff_t	buf_type;
		     Sflag_t	flags;
            	     char	*base;
            	     char	*ptr;
            	     char	*end;
            	     size_t	size;
                     Spos_t	pos;
                   };


#endif	// PL_STREAM_IMPL_H_
