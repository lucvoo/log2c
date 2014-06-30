/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "pl-hash.h"


hash_t PL_hpjw(const char *x)		// From Dragon book, p436
{
	hash_t g, h = 0;

	while (*x != 0) {
		h = (h << 4) + *x++;
		if ((g = h & 0xf0000000) != 0)
			h = (h ^ (g >> 24)) ^ g;
	}
	return h;
}
