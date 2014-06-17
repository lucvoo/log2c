/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/
/****************************************************************/

#ifndef	PL_STREAM_IMPL_H_
#define	PL_STREAM_IMPL_H_

#include "pl-stream.h"
#include "pl-buffer.h"

#ifdef	HAVE_PIPE
#include <unistd.h>			// for pid_t
#endif

union stream_handle {
	int fd;
	int sd;
	struct ubuffer *ubs;
};

typedef int (*Sread_fun) (union stream_handle hndl, void *buf, int count);
typedef int (*Swrite_fun) (union stream_handle hndl, const void *buf, int count);
typedef off_t(*Sseek_fun) (union stream_handle hndl, long pos, int whence);
typedef int (*Sclose_fun) (struct stream *S);
typedef int (*Scntl_fun) (union stream_handle hndl, int action, void *arg);

struct stream_ops {
	Sread_fun Sread;
	Swrite_fun Swrite;
	Sclose_fun Sclose;
	Sseek_fun Sseek;
	Scntl_fun Scntl;
};

struct stream {
	union stream_handle hndl;
#ifdef	HAVE_PIPE
	pid_t pid;			// for pipe stream
#endif
	enum stream_mode mode;
	enum stream_type type;
	enum stream_bufftype buf_type;
	Sflag_t flags;
	char *base;
	char *ptr;
	char *end;
	int lastc;
	size_t size;
	struct stream_ops *funs;
	struct stream_pos pos;
};

#endif
