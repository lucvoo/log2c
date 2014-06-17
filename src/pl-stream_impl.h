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

typedef union hndl_t {
	int fd;
	int sd;
	pl_ubs_t *ubs;
} Shndl_t;

typedef int (*Sread_fun) (Shndl_t hndl, void *buf, int count);
typedef int (*Swrite_fun) (Shndl_t hndl, const void *buf, int count);
typedef off_t(*Sseek_fun) (Shndl_t hndl, long pos, int whence);
typedef int (*Sclose_fun) (struct stream *S);
typedef int (*Scntl_fun) (Shndl_t hndl, int action, void *arg);

typedef struct Sfunctions {
	Sread_fun Sread;
	Swrite_fun Swrite;
	Sclose_fun Sclose;
	Sseek_fun Sseek;
	Scntl_fun Scntl;
} Sfun_t;

struct stream {
	Shndl_t hndl;
#ifdef	HAVE_PIPE
	pid_t pid;			// for pipe stream
#endif
	Smode_t mode;
	Stype_t type;
	Sbuff_t buf_type;
	Sflag_t flags;
	char *base;
	char *ptr;
	char *end;
	int lastc;
	size_t size;
	Sfun_t *funs;
	Spos_t pos;
};

#endif
