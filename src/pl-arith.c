/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-pred.h"

// FIXME : add floating number
int PL_eval_(union cell *c, long *n)
{
	struct functor *f;
	long r, n1, n2;

debut:
	switch (get_tag(c)) {

	case ref_tag:
		c = c->celp;
		goto debut;
	case var_tag:
		PL_warn("Unbound variable in arithmetic expression");
		return 0;
	case ato_tag:
		PL_warn("Unknow arithmetic operator: %s/%d", AtomName(get_atom(c)), 0);
		return 0;
	case int_tag:
		*n = get_val(c);
		return 1;
	case fun_tag:
		f = get_fun(c);
		switch (f->arity) {
		case 2:
			PL_eval_(c + 2, &n2);
		case 1:
			PL_eval_(c + 1, &n1);
			break;
		}

		// FIXME : hash-table
		if (f == FUN(plus, 2)) {
			r = n1 + n2;
		} else if (f == FUN(minus, 2)) {
			r = n1 - n2;
		} else if (f == FUN(star, 2)) {
			r = n1 * n2;
		} else if (f == FUN(div, 2)) {
			r = n1 / n2;
		} else if (f == FUN(divide, 2)) {
			r = n1 / n2;		// Incorrect for negative numbers?
		} else if (f == FUN(_mod, 2)) {
			r = n1 % n2;
		} else if (f == FUN(minus, 1)) {
			r = -n1;
		} else if (f == FUN(_max, 2)) {
			r = ((n1 > n2) ? n1 : n2);
		} else if (f == FUN(_min, 2)) {
			r = ((n1 < n2) ? n1 : n2);
		} else if (f == FUN(rshift, 2)) {
			r = n1 >> n2;
		} else if (f == FUN(lshift, 2)) {
			r = n1 << n2;
		} else {
			PL_warn("Unknow arithmetic operator: %s/%ld", FunName(f), FunArity(f));
			return 0;
		}
		*n = r;
		return 1;

	default:
		PL_warn("Invalid arithmetic expression");
		return 0;
	}
}
