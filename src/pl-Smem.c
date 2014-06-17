/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "pl-stream_impl.h"

/**************/
/* Write only */
/**************/

char *Sstring_wmem(struct stream *S)
{
	Sputc(S, '\0');
	Sflush(S);

	return (PL_base_ubs(S->hndl.ubs));
}

static int Swrite_wmem(union stream_handle hndl, const void *s, int n)
{
	PL_add_x_ubs(hndl.ubs, s, n);
	return (n);
}

static int Sclose_wmem(struct stream *S)
{
	PL_free_ubs(S->hndl.ubs);
	free(S->hndl.ubs);

	return (0);
}

static struct stream_ops wmem_functions = { 0,
	Swrite_wmem,
	Sclose_wmem,
	0,
	0,
};

struct stream *Sopen_wmem(const char *buf, enum stream_mode mode, int flags)
{
	struct stream *S;
	struct ubuffer *ubs;

	if (mode != SM_WRITE) {		// FIXME : errmsg
		return (0);
	}

	if (!(S = Snew_stream())) {	// FIXME : errmsg
		return (0);
	}

	ubs = malloc(sizeof(struct ubuffer));
	if (!ubs) {			// FIXME : errmsg
		return (0);
	} else {
		PL_init_ubs(ubs);
	}

	if (flags & SF_RECPOS) {
		S->pos.char_no = 0;
		S->pos.line_no = 1;
		S->pos.col_no = 0;
	}

	S->type = ST_WMEM;
	S->mode = mode;
	S->funs = &wmem_functions;

	if (!S_setbuf(S, 0, 248, SF_FBUF))
		return (0);

	S->hndl.ubs = ubs;
	S->flags = flags;

	return (S);
}

/*************/
/* Read only */
/*************/

static int Sread_rmem(union stream_handle hndl, void *s, int n)
{
	return (0);
}

static int Sclose_rmem(struct stream *S)
{
	return (0);
}

static struct stream_ops rmem_functions = { Sread_rmem,
	0,
	Sclose_rmem,
	0,
	0,
};

struct stream *Sopen_rmem(const char *buf, enum stream_mode mode, int flags)
{
	struct stream *S;

	if (!(S = Snew_stream())) {	// FIXME : errmsg
		return (0);
	}

	if (!buf) {			// FIXME : errmsg
		return (0);
	}

	if (flags & SF_RECPOS) {
		S->pos.char_no = 0;
		S->pos.line_no = 1;
		S->pos.col_no = 0;
	}

	S->type = ST_RMEM;
	S->mode = mode;
	S->funs = &rmem_functions;

	{
		int size = 0;
		const char *ptr = buf;

		while (*ptr++)
			size++;		// This is strlen()

		if (!S_setbuf(S, (char *)buf, size, SF_FBUF))
			return (0);
		flags |= SF_STATIC;
	}

	S->hndl.fd = -1;
	S->flags = flags;

	return (S);
}
