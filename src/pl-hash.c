/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "pl-hash.h"
#include <stdint.h>


// From Dragon book, p436
static uint32_t hpjw(uint32_t iv, unsigned int c)
{
	uint32_t h = iv;
	uint32_t g;

	h = (h << 4) + c;
	if ((g = h & 0xf0000000) != 0)
		h = (h ^ (g >> 24)) ^ g;

	return h;
}

static uint32_t hpjw_str(uint32_t iv, const char *x)
{
	const unsigned char *s = (const void *)x;
	unsigned int c;
	uint32_t h = iv;

	while ((c = *s++) != 0) {
		h = hpjw(h, c);
	}

	return h;
}

static uint32_t hpjw_mem(uint32_t iv, const void *x, unsigned int n)
{
	const unsigned char *s = x;
	uint32_t h = iv;

	while (n--) {
		h = hpjw(h, *s++);
	}

	return h;
}

hash_t PL_hash_str(const char *x)
{
	return hpjw_str(0, x);
}

hash_t PL_hash_mem(const void *x, unsigned int len)
{
	return hpjw_mem(0, x, len);
}
