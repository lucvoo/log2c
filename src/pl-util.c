/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include <stdlib.h>			// for malloc(3)
#include "pl-string.h"
#include "pl-ctype.h"
#include "pl-fli.h"
#include "pl-atom.h"

typedef struct {
	char ul[2];
} hexdigit_t;

static hexdigit_t HexName[256] =
	{ {{'0', '0'}}, {{'0', '1'}}, {{'0', '2'}}, {{'0', '3'}}, {{'0', '4'}}, {{'0', '5'}},
	{{'0', '6'}}, {{'0', '7'}}, {{'0', '8'}}, {{'0', '9'}}, {{'0', 'A'}}, {{'0', 'B'}},
	{{'0', 'C'}}, {{'0', 'D'}}, {{'0', 'E'}}, {{'0', 'F'}},
{{'1', '0'}}, {{'1', '1'}}, {{'1', '2'}}, {{'1', '3'}}, {{'1', '4'}}, {{'1', '5'}}, {{'1', '6'}},
	{{'1', '7'}}, {{'1', '8'}}, {{'1', '9'}}, {{'1', 'A'}}, {{'1', 'B'}}, {{'1', 'C'}},
	{{'1', 'D'}}, {{'1', 'E'}}, {{'1', 'F'}},
{{'2', '0'}}, {{'2', '1'}}, {{'2', '2'}}, {{'2', '3'}}, {{'2', '4'}}, {{'2', '5'}}, {{'2', '6'}},
	{{'2', '7'}}, {{'2', '8'}}, {{'2', '9'}}, {{'2', 'A'}}, {{'2', 'B'}}, {{'2', 'C'}},
	{{'2', 'D'}}, {{'2', 'E'}}, {{'2', 'F'}},
{{'3', '0'}}, {{'3', '1'}}, {{'3', '2'}}, {{'3', '3'}}, {{'3', '4'}}, {{'3', '5'}}, {{'3', '6'}},
	{{'3', '7'}}, {{'3', '8'}}, {{'3', '9'}}, {{'3', 'A'}}, {{'3', 'B'}}, {{'3', 'C'}},
	{{'3', 'D'}}, {{'3', 'E'}}, {{'3', 'F'}},
{{'4', '0'}}, {{'4', '1'}}, {{'4', '2'}}, {{'4', '3'}}, {{'4', '4'}}, {{'4', '5'}}, {{'4', '6'}},
	{{'4', '7'}}, {{'4', '8'}}, {{'4', '9'}}, {{'4', 'A'}}, {{'4', 'B'}}, {{'4', 'C'}},
	{{'4', 'D'}}, {{'4', 'E'}}, {{'4', 'F'}},
{{'5', '0'}}, {{'5', '1'}}, {{'5', '2'}}, {{'5', '3'}}, {{'5', '4'}}, {{'5', '5'}}, {{'5', '6'}},
	{{'5', '7'}}, {{'5', '8'}}, {{'5', '9'}}, {{'5', 'A'}}, {{'5', 'B'}}, {{'5', 'C'}},
	{{'5', 'D'}}, {{'5', 'E'}}, {{'5', 'F'}},
{{'6', '0'}}, {{'6', '1'}}, {{'6', '2'}}, {{'6', '3'}}, {{'6', '4'}}, {{'6', '5'}}, {{'6', '6'}},
	{{'6', '7'}}, {{'6', '8'}}, {{'6', '9'}}, {{'6', 'A'}}, {{'6', 'B'}}, {{'6', 'C'}},
	{{'6', 'D'}}, {{'6', 'E'}}, {{'6', 'F'}},
{{'7', '0'}}, {{'7', '1'}}, {{'7', '2'}}, {{'7', '3'}}, {{'7', '4'}}, {{'7', '5'}}, {{'7', '6'}},
	{{'7', '7'}}, {{'7', '8'}}, {{'7', '9'}}, {{'7', 'A'}}, {{'7', 'B'}}, {{'7', 'C'}},
	{{'7', 'D'}}, {{'7', 'E'}}, {{'7', 'F'}},
{{'8', '0'}}, {{'8', '1'}}, {{'8', '2'}}, {{'8', '3'}}, {{'8', '4'}}, {{'8', '5'}}, {{'8', '6'}},
	{{'8', '7'}}, {{'8', '8'}}, {{'8', '9'}}, {{'8', 'A'}}, {{'8', 'B'}}, {{'8', 'C'}},
	{{'8', 'D'}}, {{'8', 'E'}}, {{'8', 'F'}},
{{'9', '0'}}, {{'9', '1'}}, {{'9', '2'}}, {{'9', '3'}}, {{'9', '4'}}, {{'9', '5'}}, {{'9', '6'}},
	{{'9', '7'}}, {{'9', '8'}}, {{'9', '9'}}, {{'9', 'A'}}, {{'9', 'B'}}, {{'9', 'C'}},
	{{'9', 'D'}}, {{'9', 'E'}}, {{'9', 'F'}},
{{'A', '0'}}, {{'A', '1'}}, {{'A', '2'}}, {{'A', '3'}}, {{'A', '4'}}, {{'A', '5'}}, {{'A', '6'}},
	{{'A', '7'}}, {{'A', '8'}}, {{'A', '9'}}, {{'A', 'A'}}, {{'A', 'B'}}, {{'A', 'C'}},
	{{'A', 'D'}}, {{'A', 'E'}}, {{'A', 'F'}},
{{'B', '0'}}, {{'B', '1'}}, {{'B', '2'}}, {{'B', '3'}}, {{'B', '4'}}, {{'B', '5'}}, {{'B', '6'}},
	{{'B', '7'}}, {{'B', '8'}}, {{'B', '9'}}, {{'B', 'A'}}, {{'B', 'B'}}, {{'B', 'C'}},
	{{'B', 'D'}}, {{'B', 'E'}}, {{'B', 'F'}},
{{'C', '0'}}, {{'C', '1'}}, {{'C', '2'}}, {{'C', '3'}}, {{'C', '4'}}, {{'C', '5'}}, {{'C', '6'}},
	{{'C', '7'}}, {{'C', '8'}}, {{'C', '9'}}, {{'C', 'A'}}, {{'C', 'B'}}, {{'C', 'C'}},
	{{'C', 'D'}}, {{'C', 'E'}}, {{'C', 'F'}},
{{'D', '0'}}, {{'D', '1'}}, {{'D', '2'}}, {{'D', '3'}}, {{'D', '4'}}, {{'D', '5'}}, {{'D', '6'}},
	{{'D', '7'}}, {{'D', '8'}}, {{'D', '9'}}, {{'D', 'A'}}, {{'D', 'B'}}, {{'D', 'C'}},
	{{'D', 'D'}}, {{'D', 'E'}}, {{'D', 'F'}},
{{'E', '0'}}, {{'E', '1'}}, {{'E', '2'}}, {{'E', '3'}}, {{'E', '4'}}, {{'E', '5'}}, {{'E', '6'}},
	{{'E', '7'}}, {{'E', '8'}}, {{'E', '9'}}, {{'E', 'A'}}, {{'E', 'B'}}, {{'E', 'C'}},
	{{'E', 'D'}}, {{'E', 'E'}}, {{'E', 'F'}},
{{'F', '0'}}, {{'F', '1'}}, {{'F', '2'}}, {{'F', '3'}}, {{'F', '4'}}, {{'F', '5'}}, {{'F', '6'}},
	{{'F', '7'}}, {{'F', '8'}}, {{'F', '9'}}, {{'F', 'A'}}, {{'F', 'B'}}, {{'F', 'C'}},
	{{'F', 'D'}}, {{'F', 'E'}}, {{'F', 'F'}},
};

int PL__DigitValue[256] = { 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36,
	36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36,
	36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36,
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 36, 36, 36, 36, 36, 36,
	0, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
	25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 36, 36, 36, 36,
	0, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
	25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 36, 36, 36, 36,
	36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36,
	36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36,
	36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36,
	36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36,
	36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36,
	36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36,
	36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36,
	36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36,
};

/* mangle a string to a symbol name
   // return a malloced string
*/

inline static char *PL_mangle(const char *src)
{
	void *old_HP = HP;
	char *dst = (char *)HP;

	*dst++ = '_';
	HP = (void *)dst;
	while (*src) {
		if (*src == '_') {
			dst[0] = dst[1] = *src++;
			dst += 2;
			HP = (void *)dst;
		} else if (isAlphaNum(*src)) {
			*dst++ = *src++;
			HP = (void *)dst;
		} else			// not alphanum nor underscore
		{
			hexdigit_t *xd;
			dst[0] = '_';
			xd = (hexdigit_t *) (dst + 1);
			*xd = HexName[(unsigned int)*src];
			src++;
			dst += 3;
			HP = (void *)dst;
		}
	}
	*dst = '\0';

#undef dst

	HP = old_HP;
	return (old_HP);
}

int pl_mangle(union cell *name, union cell *mangled)
{
	const char *n, *m;
	struct atom *a;

	if (!PL_get_atom_chars(name, &n))
		fail;

	m = PL_mangle(n);
	a = PL_new_atom(m);
	return PL_unify_atom(mangled, a);
}
