/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_BUFFER_H_
#define PL_BUFFER_H_

#include "Prolog.h"
#include <malloc.h>
#include "pl-string.h"


/**********************************************************************/
/* Unbound buffer string                                              */
/**********************************************************************/

typedef struct { char *base;
                 char *ptr;
                 char *end;
               } ubs_t;

INLINE_DECL
void init_ubs(ubs_t *ubs)
{ char *s=malloc(240);			// FIXME : test if fail
  ubs->ptr=ubs->base=s;
  ubs->end=s+240;
}

INLINE_DECL
void add_ubs(ubs_t *b, int c)
{ if (b->ptr==b->end)
  { size_t size=(b->end-b->base)+256;
    char *s=realloc(b->base,size);		// FIXME : test if fail
    b->ptr=s+(b->ptr-b->base);
    b->base=s;
    b->end=s+size;
  }
  *(b->ptr)++=c;
}

INLINE_DECL
void add_x_ubs(ubs_t *b, const char *s, size_t n)
{ if ((b->end-b->ptr)<=n)
  { size_t size=(b->end-b->base)+round_to_power(n+1);
    char *s=realloc(b->base,size);		// FIXME : test if fail
    b->ptr=s+(b->ptr-b->base);
    b->base=s;
    b->end=s+size;
  }
  memcpy(b->ptr,s,n);
  b->ptr+=n;
}

INLINE_DECL
char *base_ubs(ubs_t *b)
{ add_ubs(b,'\0');
  (b->ptr)--;
  return(b->base);
}

INLINE_DECL
void free_ubs(ubs_t *b)
{ free(b->base); }

INLINE_DECL
void clear_ubs(ubs_t *b)
{ b->ptr=b->base; }

/**********************************************************************/
/* Ring Buffers                                                       */
/**********************************************************************/

ubs_t *find_ubs(unsigned flags);
void unfind_ubs(unsigned flags);

#ifndef BUF_RING
#define BUF_DISCARDABLE 0
#define BUF_MALLOC	(1<<8)
#define BUF_RING	(1<<9)
#endif

#endif	// PL_BUFFER_H_
