/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/
/****************************************************************/

#ifndef	PL_STREAM_H_
#define	PL_STREAM_H_

#include <stddef.h>			// for size_t
#include "pl-config.h"

#define S_EOF	(-1)

enum stream_mode { SM_READ, SM_WRITE, SM_APPEND, SM_UPDATE, SM_ANY };
enum stream_type { ST_FILE, ST_PIPE, ST_RMEM, ST_WMEM, ST_SOCK };
enum stream_bufftype { SB_NONE, SB_LINE, SB_FULL };
typedef unsigned long Sflag_t;

#define SFlags(n)	(1<<n)

#define SF_ERR		SFlags(0)
#define SF_EOF		SFlags(1)
#define SF_EOF2		SFlags(2)
#define SF_EOF_ERR	SFlags(3)
#define SF_EOF_RESET	SFlags(4)
#define SF_RECPOS	SFlags(5)
#define SF_BINARY	SFlags(6)
#define SF_OPEN		SFlags(7)
#define SF_STATIC	SFlags(8)
#define	SF_NBUF		SFlags(9)
#define SF_LBUF		SFlags(10)
#define	SF_FBUF		SFlags(11)
#define SF_BUFFERING	(SF_NBUF|SF_LBUF|SF_FBUF)

#define	SF_SEEK_SET	SEEK_SET
#define	SF_SEEK_CUR	SEEK_CUR
#define	SF_SEEK_END	SEEK_END

// #define S_BUFSIZ     8196
#define S_BUFSIZ	8

struct stream;

struct stream_pos {
	long char_no;
	long line_no;
	long col_no;
};

extern struct stream *Stdin;
extern struct stream *Stdout;
extern struct stream *Stderr;

int S_setbuf(struct stream *S, char *buf, size_t size, int buf_type);
struct stream *Snew_stream(void);
void Sfree_stream(struct stream *S);
int Sgetc(struct stream *S);
int Sungetc(struct stream *S, int c);
int Speekc(struct stream *S);
int Sputc(struct stream *S, int c);
int Slastc(struct stream *S);
void Sclearerr(struct stream *S);
int Serror(struct stream *S);
int Sflush(struct stream *S);
int Sclose(struct stream *S);
int Sputs(struct stream *S, const char *s);
struct stream *Sopen_file(const char *file, enum stream_mode mode, int flags);
struct stream *Sopen_wmem(const char *str, enum stream_mode mode, int flags);
struct stream *Sopen_rmem(const char *str, enum stream_mode mode, int flags);
char *Sstring_wmem(struct stream *S);
struct stream *Sopen_pipe(const char *cmd, enum stream_mode mode, int flags);
long Stell(struct stream *);
int Sseek(struct stream *, long, int);
int Sfprintf(struct stream *, const char *, ...) __attribute__ ((format(printf, 2, 3)));

void pl_init_stream(void);

enum stream_type StreamType(struct stream *);
enum stream_mode StreamMode(struct stream *);
Sflag_t StreamFlags(struct stream *);
struct stream_pos *Sget_pos(struct stream *);
int Seof(struct stream *S);
int Spasteof(struct stream *S);

#endif
