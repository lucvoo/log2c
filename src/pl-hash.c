/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "pl-hash.h"

hash_t PL_hpjw(const char* x) // From Dragon book, p436
{ hash_t g, h = 0;

  while (*x != 0)
  { h = (h << 4) + *x++;
    if ((g = h & 0xf0000000) != 0)
      h = (h ^ (g >> 24)) ^ g;
  }
  return h;
}

hash_t PL_hpjw_2(const char* x, const char *y) // From Dragon book, p436
{ hash_t g, h = 0;

  while (*x != 0)
  { h = (h << 4) + *x++;
    if ((g = h & 0xf0000000) != 0)
      h = (h ^ (g >> 24)) ^ g;
  }
  while (*y != 0)
  { h = (h << 4) + *y++;
    if ((g = h & 0xf0000000) != 0)
      h = (h ^ (g >> 24)) ^ g;
  }
  return h;
}

hash_t PL_mult_hash(int x)
{ // uses a const close to golden ratio * pow(2,32)
  return ((hash_t)x) * 2654435767UL;
}


