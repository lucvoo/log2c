/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "pl-buffer.h"

static unsigned int current_buffer_no;
static ubs_t ring_buffers[MAX_RING_BUFFER];
static ubs_t discardable_buffer;

ubs_t *find_ubs(unsigned flags)
{ ubs_t *b;

  if (flags & BUF_RING)
  { current_buffer_no = (current_buffer_no+1) % MAX_RING_BUFFER;
    b=&ring_buffers[current_buffer_no];
  }
  else
    b=&discardable_buffer;

  clear_ubs(b);
  return(b);
}

void unfind_ubs(unsigned flags)
{ if (flags & BUF_RING)
    current_buffer_no = (current_buffer_no-1) % MAX_RING_BUFFER;
}

