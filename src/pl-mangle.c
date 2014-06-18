/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-ctype.h"


static int hexdigit(unsigned int c)
{
	if (c <= 9)
		return c + '0';
	else
		return c + ('A' - 10);
}

/**
 * Mangle a string to a valid C/asm symbol name.
 * @return the result string on the HEAP
*/
static char *do_mangle(const char *src)
{
	char *dst = (void *)HP;
	unsigned int c;

	*dst++ = '_';
	while ((c = *((const unsigned char *)src++))) {
		if (c == '_') {
			dst[0] = dst[1] = c;
			dst += 2;
		} else if (isAlphaNum(c)) {
			*dst++ = c;
		} else {		// not alphanum nor underscore
			dst[0] = '_';
			dst[1] = hexdigit(c / 16);
			dst[2] = hexdigit(c & 15);
			dst += 3;
		}
	}
	*dst = '\0';

	return (char *)HP;
}

/**
 * pl_mangle(+atom, -atom) is det
 * pl_mangle(+atom, +atom) is semidet
 *
 * Convert an atom to another one corresponding to its mangled form (valid C/asm symbol name).
*/
int pl_mangle(union cell *name, union cell *mangled)
{
	const char *n, *m;
	struct atom *a;

	if (!PL_get_atom_chars(name, &n))
		fail;

	m = do_mangle(n);
	a = PL_new_atom(m);
	return PL_unify_atom(mangled, a);
}
