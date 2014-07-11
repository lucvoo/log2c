/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef	PL_BUFFER_H_
#define PL_BUFFER_H_

#include "Prolog.h"
#include "pl-string.h"
#include <stdlib.h>			// For realloc(3)

/**********************************************************************/
/* Unbound buffer string                                              */
/**********************************************************************/

struct ubuffer {
	char *base;
	char *ptr;
	char *end;
};

inline static void PL_add_ubs(struct ubuffer *b, int c)
{
	if (b->ptr == b->end) {
		size_t size = (b->end - b->base) + 256;
		char *s = realloc(b->base, size);	// FIXME : test if fail
		b->ptr = s + (b->ptr - b->base);
		b->base = s;
		b->end = s + size;
	}
	*(b->ptr)++ = c;
}

inline static void PL_add_x_ubs(struct ubuffer *b, const char *s, size_t n)
{
	if ((b->end - b->ptr) <= n) {
		size_t size = (b->end - b->base) + round_to_power(n + 1);
		char *s = realloc(b->base, size);	// FIXME : test if fail
		b->ptr = s + (b->ptr - b->base);
		b->base = s;
		b->end = s + size;
	}
	memcpy(b->ptr, s, n);
	b->ptr += n;
}

inline static void PL_sub_x_ubs(struct ubuffer *b, size_t n)
{
	b->ptr -= n;
}

inline static char *PL_base_ubs(struct ubuffer *b)
{
	PL_add_ubs(b, '\0');
	(b->ptr)--;
	return b->base;
}

inline static void PL_clear_ubs(struct ubuffer *b)
{
	b->ptr = b->base;
}

/**********************************************************************/
/* Ring Buffers                                                       */
/**********************************************************************/

void PL_free_ubs(struct ubuffer *b);
void PL_init_ubs(struct ubuffer *ubs);
struct ubuffer *PL_find_ubs(unsigned flags);
void PL_lost_ubs(unsigned flags);

#ifndef BUF_RING
#define BUF_DISCARDABLE 0
#define BUF_MALLOC	(1<<8)
#define BUF_RING	(1<<9)
#endif

#endif
