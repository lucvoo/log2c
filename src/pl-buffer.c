/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "pl-buffer.h"

static unsigned int current_buffer_no;
static pl_ubs_t ring_buffers[MAX_RING_BUFFER];
static pl_ubs_t discardable_buffer;

pl_ubs_t *PL_find_ubs(unsigned flags)
{ pl_ubs_t *b;

  if (flags & BUF_RING)
  { current_buffer_no = (current_buffer_no+1) % MAX_RING_BUFFER;
    b=&ring_buffers[current_buffer_no];
  }
  else
    b=&discardable_buffer;

  PL_clear_ubs(b);
  return(b);
}

void PL_unfind_ubs(unsigned flags)
{ if (flags & BUF_RING)
    current_buffer_no = (current_buffer_no-1) % MAX_RING_BUFFER;
}

