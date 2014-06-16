/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/
/****************************************************************/

#ifndef	PL_STREAM_H_
#define	PL_STREAM_H_

#include <stddef.h>		// for size_t
#include "pl-config.h"


#define S_EOF	(-1)

typedef enum { SM_READ, SM_WRITE, SM_APPEND, SM_UPDATE, SM_ANY } Smode_t;
typedef enum { ST_FILE, ST_PIPE, ST_RMEM, ST_WMEM, ST_SOCK } Stype_t;
typedef enum { SB_NONE, SB_LINE, SB_FULL } Sbuff_t;
typedef unsigned long	Sflag_t;


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


// #define S_BUFSIZ	8196
#define S_BUFSIZ	8

#ifndef	STRUCT_PL_STREAM
#define STRUCT_PL_STREAM
typedef struct pl_stream_t pl_stream_t, *pl_stream;
#endif

typedef struct { long	 char_no;
		 long	 line_no;
		 long	 col_no;
	       } Spos_t;


extern pl_stream Stdin;
extern pl_stream Stdout;
extern pl_stream Stderr;

int		S_setbuf(pl_stream S, char *buf, size_t size, int buf_type);
pl_stream	Snew_stream(void);
void		Sfree_stream(pl_stream S);
int 		Sgetc(pl_stream S);
int 		Sungetc(pl_stream S, int c);
int 		Speekc(pl_stream S);
int 		Sputc(pl_stream S, int c);
int		Slastc(pl_stream S);
void		Sclearerr(pl_stream S);
int		Serror(pl_stream S);
int		Sflush(pl_stream S);
int		Sclose(pl_stream S);
int		Sputs(pl_stream S, const char *s);
pl_stream	Sopen_file(const char *file, Smode_t mode, int flags);
pl_stream	Sopen_wmem(const char *str, Smode_t mode, int flags);
pl_stream	Sopen_rmem(const char *str, Smode_t mode, int flags);
char 		*Sstring_wmem(pl_stream S);
pl_stream	Sopen_pipe(const char *cmd, Smode_t mode, int flags);
long		Stell(pl_stream);
int		Sseek(pl_stream, long, int);
int		Sfprintf(pl_stream, const char *, ...) __attribute__((format (printf, 2, 3)));

void		pl_init_stream(void);

Stype_t		StreamType(pl_stream);
Smode_t		StreamMode(pl_stream);
Sflag_t		StreamFlags(pl_stream);
Spos_t *	Sget_pos(pl_stream);
int		Seof(pl_stream S);
int		Spasteof(pl_stream S);

#endif	// PL_STREAM_H_
