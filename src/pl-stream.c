/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "pl-stream.h"
#include "pl-stream_impl.h"

#include <stdlib.h>			// For malloc and friends

#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

#define inline

//#####################################################################

inline static int Set_error(struct stream *S)
{
	S->flags |= SF_ERR;
	S->ptr = S->end = S->base;
	return S_EOF;
}

struct stream *Snew_stream(void)
{
	struct stream *S;

	return malloc(sizeof(S[0]));
}

//#####################################################################

struct stream_pos *Sget_pos(struct stream *S)
{
	if (S->flags & SF_RECPOS)
		return &(S->pos);
	else
		return 0;
}

enum stream_type StreamType(struct stream *S)
{
	return S->type;
}

enum stream_mode StreamMode(struct stream *S)
{
	return S->mode;
}

Sflag_t StreamFlags(struct stream *S)
{
	return S->flags;
}

//#####################################################################

inline static int S_update_pos(struct stream *S, int c)
{
	struct stream_pos *p;

	p = Sget_pos(S);
	if (p) {
		switch (c) {
		case '\n':
			p->line_no++;
			p->col_no = 0;
			break;
		case '\r':
			p->col_no = 0;
			break;
		case '\b':
			if (p->col_no > 0)
				p->col_no--;
			break;
		case S_EOF:
			return S_EOF;
		case '\t':
			p->col_no |= 7;
		default:
			p->col_no++;
		}
		p->char_no++;
	}

	return c;
}

//#####################################################################

// RETURN : S_EOF if eof or error; first char otherwise
static int Sfillbuf(struct stream *S)
{
	if ((S->mode != SM_READ) || Serror(S)) {	// FIXME : msg
		return Set_error(S);
	}

	if (S->flags & SF_EOF) {
		S->flags |= SF_EOF2;
		if (S->flags & SF_EOF_ERR)
			Set_error(S);

		return S_EOF;
	}

	if (S->ops->read) {
		int r;

		r = S->ops->read(S->hndl, S->base, S->size);
		if (r > 0) {
			S->ptr = S->base;
			S->end = S->base + r;
			return S->base[0];
		} else if (r == 0) {
			S->ptr = S->end = S->base;
			if (!(S->flags & SF_EOF_RESET))
				S->flags |= SF_EOF;
			return S_EOF;
		} else			// r<0
		{
			return Set_error(S);
		}
	} else {			// FIXME : errmsg
		return S_EOF;
	}

}

int Sgetc(struct stream *S)
{
	int c;
	if (S->ptr < S->end)
		c = *S->ptr++;
	else {
		c = Sfillbuf(S);
		if (!(c < 0))
			S->ptr++;
	}

	return S_update_pos(S, c);
}

int Speekc(struct stream *S)
{
	int c;
	if (S->ptr < S->end)
		c = *S->ptr;
	else
		c = Sfillbuf(S);

	return c;
}

//#####################################################################

// RETURN : S_EOF on error; 0 otherwise.
static int S_flushbuf(struct stream *S)
{

	if (Serror(S)) {
		return S_EOF;
	}

	if (S->ops->write) {
		int count = S->ptr - S->base;
		if (count > 0) {
			int w = S->ops->write(S->hndl, S->base, count);
			if (w == -1)
				return Set_error(S);

			S->lastc = S->ptr[-1];
			S->ptr = S->base;
		}
		return 0;
	} else {
		return S_EOF;
	}
}

int Sputc(struct stream *S, int c)
{
	int rval;

	if (S->mode == SM_READ) {	// FIXME : msg
		return S_EOF;
	}

	*S->ptr++ = (unsigned char)c;

	if ((S->ptr == S->end) || ((S->buf_type == SB_LINE) && c == '\n')) {
		if ((rval = S_flushbuf(S)) < 0) {
			return rval;
		}
	}

	return S_update_pos(S, c);
}

int Sputs(struct stream *S, const char *s)
{

	if (S->mode == SM_READ) {	// FIXME : msg
		return S_EOF;
	}
	// FIXME : can be optimized !
	while (*s)
		Sputc(S, *s++);

	return !Serror(S);
}

int Slastc(struct stream *S)
{
	if (S->ptr == S->base)
		return S->lastc;
	else
		return S->ptr[-1];
}

//#####################################################################

int Sungetc(struct stream *S, int c)
{
	if (S->ptr > S->base) {
		if (S->flags & SF_STATIC) {
			if (S->ptr[-1] == (unsigned char)c)
				return c;
			else
				return Set_error(S);
		} else {
			*--S->ptr = (unsigned char)c;
			return c;
		}
	} else
		return Set_error(S);
}

//#####################################################################

void Sclearerr(struct stream *S)
{
	S->flags &= ~(SF_ERR | SF_EOF | SF_EOF2);
}

int Seof(struct stream *S)
{
	return S->flags & SF_EOF;
}

int Serror(struct stream *S)
{
	return S->flags & SF_ERR;
}

int Spasteof(struct stream *S)
{
	return (S->flags & (SF_EOF2 | SF_EOF_ERR)) == (SF_EOF2 | SF_EOF_ERR);
}

int Sflush(struct stream *S)
{
	if (S->mode == SM_READ)
		return 0;
	else
		return S_flushbuf(S);
}

//#####################################################################

int Sclose(struct stream *S)
{
	int rval = 0;

	rval = Sflush(S);
	if (S == Stdin || S == Stdout || S == Stderr)
		return rval;

	if (!(S->flags & SF_STATIC))
		free(S->base);

	rval = S->ops->close(S);
	free(S);

	return rval;
}

//#####################################################################

// PRE : type = 0 | SF_NBUF | SF_LBUF | SF_FBUF
// FIXME : static
int S_setbuf(struct stream *S, char *buf, size_t size, int type)
{
	enum stream_bufftype buf_type;
	int static_buf = 0;

	if (!buf) {
		if (size <= 0) {	/* Must calculate size */
			struct stat st;

			if (fstat(S->hndl.fd, &st) == 0)
				size = st.st_blksize <= 0 ? S_BUFSIZ : st.st_blksize;
			else
				size = S_BUFSIZ;	// default size
		}

		if (type == SF_NBUF) {
			size = 1;
		}

		buf = malloc(size);
		if (!buf)
			return 0;
	} else {
		static_buf = 1;
	}

/* Choose the buffering type */
	switch (type) {
	case 0:
		buf_type = isatty(S->hndl.fd) ? SB_LINE : SB_FULL;
		break;
	case SF_NBUF:
		buf_type = SB_NONE;
		break;
	case SF_LBUF:
		buf_type = SB_LINE;
		break;
	case SF_FBUF:
		buf_type = SB_FULL;
		break;
	default:
		// FIXME : msg
		return 0;
	}

	S->buf_type = buf_type;
	S->base = S->ptr = buf;
	if (S->mode == SM_READ && !static_buf)
		S->end = S->base;
	else
		S->end = S->base + size;
	S->size = size;
	S->lastc = -1;

	return 1;
}

//#####################################################################

long Stell(struct stream *S)
{
	if (S->ops->seek) {
		long l;
		l = S->ops->seek(S->hndl, 0, SEEK_CUR);
		if (S->mode == SM_READ) {
			l -= S->end - S->ptr;
		} else {
			l += S->ptr - S->base;
		}
		return l;
	} else
		return -1;
}

int Sseek(struct stream *S, long off, int whence)
{
	if (S->ops->seek) {
		long l;
		// FIXME !!!
		S->ptr = S->end;	// Will force a fillbuf
		l = S->ops->seek(S->hndl, off, whence);
		return l >= 0 ? 0 : -1;
	} else
		return -1;
}
