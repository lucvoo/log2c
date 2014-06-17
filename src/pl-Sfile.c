/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "pl-stream_impl.h"

#include <fcntl.h>

static int Swrite_file(Shndl_t hndl, const void *s, int n)
{
	return (write(hndl.fd, s, n));
}

static int Sread_file(Shndl_t hndl, void *s, int n)
{
	return (read(hndl.fd, s, n));
}

static int Sclose_file(struct stream *S)
{
	return (close(S->hndl.fd));
}

static off_t Sseek_file(Shndl_t hndl, long off, int whence)
{
	return (lseek(hndl.fd, off, whence));
}

static Sfun_t file_functions = { Sread_file,
	Swrite_file,
	Sclose_file,
	Sseek_file,
	0,
};

struct stream *Sopen_file(const char *file, enum stream_mode mode, int flags)
{
	int fd;
	struct stream *S;
	int o_flags = 0;

// FIXME : what to do with SF_BINARY flag ???

	flags |= SF_RECPOS;

	switch (mode) {
	case SM_READ:
		o_flags |= O_RDONLY;
		break;
	case SM_WRITE:
		o_flags |= O_WRONLY | O_TRUNC | O_CREAT;
		break;
	case SM_UPDATE:
		o_flags |= O_WRONLY | O_CREAT;
		break;
	case SM_APPEND:
		o_flags |= O_APPEND | O_WRONLY | O_CREAT;
		break;
	default:
		return (0);		// impossible error
	}

	fd = open(file, o_flags, 0666);
	if (fd == -1 || !(S = Snew_stream())) {
		return (0);		// FIXME : msg
	}
	S->hndl.fd = fd;

	// if (flags & SF_RECPOS)
	{
		S->pos.char_no = 0;
		S->pos.line_no = 1;
		S->pos.col_no = 0;
	}

	S->type = ST_FILE;
	S->mode = mode;
	S->funs = &file_functions;
	if (!S_setbuf(S, 0, 0, (flags & SF_BUFFERING)))
		return (0);

	flags &= ~(SF_BUFFERING);
	S->flags = flags;		// FIXME

	return (S);
}

static struct stream Stdin__;
static struct stream Stdout__;
static struct stream Stderr__;

struct stream *Stdin = &Stdin__;
struct stream *Stdout = &Stdout__;
struct stream *Stderr = &Stderr__;

void pl_init_stream(void)
{
	static char buf_err[1];

	Stdin__.hndl.fd = STDIN_FILENO;
	Stdin__.funs = &file_functions;
	Stdin__.mode = SM_READ;
	Stdin__.type = ST_FILE;
	Stdin__.flags = SF_EOF_RESET;
	S_setbuf(&Stdin__, 0, 0, 0);

	Stdout__.hndl.fd = STDOUT_FILENO;
	Stdout__.funs = &file_functions;
	Stdout__.mode = SM_WRITE;
	Stdout__.type = ST_FILE;
	Stdout__.flags = 0;
	S_setbuf(&Stdout__, 0, 0, 0);

	Stderr__.hndl.fd = STDERR_FILENO;
	Stderr__.funs = &file_functions;
	Stderr__.mode = SM_WRITE;
	Stderr__.type = ST_FILE;
	Stderr__.flags = SF_STATIC;
	S_setbuf(&Stderr__, buf_err, sizeof(buf_err), SF_NBUF);
}
