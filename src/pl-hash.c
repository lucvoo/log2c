/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "pl-hash.h"
#include <stdint.h>


hash_t PL_hpjw(const char *x)		// From Dragon book, p436
{
	const unsigned char *s = (const void *)x;
	unsigned int c;
	uint32_t h = 0;

	while ((c = *s++) != 0) {
		uint32_t g;

		h = (h << 4) + c;
		if ((g = h & 0xf0000000) != 0)
			h = (h ^ (g >> 24)) ^ g;
	}
	return h;
}
