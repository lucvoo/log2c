/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "pl-stream_impl.h"

#include <sys/socket.h>

static int Swrite_sock(union stream_handle hndl, const void *s, int n)
{
	return send(hndl.sd, s, n, 0);
}

static int Sread_sock(union stream_handle hndl, void *s, int n)
{
	return recv(hndl.sd, s, n, 0);
}

static int Sclose_r_sock(struct stream *S)
{					// FIXME
	return 0;
}

static int Sclose_w_sock(struct stream *S)
{					// FIXME
	return 0;
}

static off_t Sseek_sock(union stream_handle hndl, long off, int whence)
{
	return -1;
}

static struct stream_ops sock_r_ops = {
	.read = Sread_sock,
	.write = Swrite_sock,
	.close = Sclose_r_sock,
	.seek = Sseek_sock,
};

static struct stream_ops sock_w_ops = {
	.read = Sread_sock,
	.write = Swrite_sock,
	.close = Sclose_w_sock,
	.seek = Sseek_sock,
};

struct stream *Sopen_sock(const char *file, enum stream_mode mode, int flags)
{
	struct stream *S;

// FIXME : what to do with SF_BINARY flag ???

	S = Snew_stream();
	if (!S) {			// FIXME : errmsg
		return 0;
	}
	// if (flags & SF_RECPOS)
	{
		S->pos.char_no = 0;
		S->pos.line_no = 1;
		S->pos.col_no = 0;
	}

	S->type = ST_SOCK;
	S->mode = mode;
	switch (mode) {
	case SM_READ:
		S->ops = &sock_r_ops;
		break;
	case SM_WRITE:
		S->ops = &sock_w_ops;
		break;
	default:			// FIXME : errmsg
		return 0;
	}
	if (!S_setbuf(S, 0, 0, (flags & SF_BUFFERING)))
		return 0;

	flags &= ~(SF_BUFFERING);
	S->flags = flags;		// FIXME

	return S;
}
