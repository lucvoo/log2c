/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "pl-buffer.h"
#include <stdlib.h>			// For malloc(3)

static unsigned int current_buffer_no;
static pl_ubs_t ring_buffers[PL_MAX_RING_BUF];
static pl_ubs_t discardable_buffer;


void PL_init_ubs(pl_ubs_t *ubs)
{ char *s=malloc(240);			// FIXME : test if fail
  ubs->ptr=ubs->base=s;
  ubs->end=s+240;
}

void PL_free_ubs(pl_ubs_t *b)
{ free(b->base);
}

pl_ubs_t *PL_find_ubs(unsigned flags)
{ pl_ubs_t *b;

  if (flags & BUF_RING)
  { current_buffer_no = (current_buffer_no+1) % PL_MAX_RING_BUF;
    b=&ring_buffers[current_buffer_no];
  }
  else
    b=&discardable_buffer;

  PL_clear_ubs(b);
  return(b);
}

void PL_lost_ubs(unsigned flags)
{ if (flags & BUF_RING)
    current_buffer_no = (current_buffer_no-1) % PL_MAX_RING_BUF;
}

