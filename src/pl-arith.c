/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-pred.h"

// FIXME : add floating number
int PL_eval_(union cell *c, int *n)
{
	int n2;				//, n2;

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
	case fun_tag:{
			struct functor *f = get_fun(c);	// FIXME : hash-table
			if (f == FUN(plus, 2)) {
				if (PL_eval_(c + 1, n) && PL_eval_(c + 2, &n2)) {
					*n += n2;
					succeed;
				} else
					fail;
			} else if (f == FUN(minus, 2)) {
				if (PL_eval_(c + 1, n) && PL_eval_(c + 2, &n2)) {
					*n -= n2;
					succeed;
				} else
					fail;
			} else if (f == FUN(star, 2)) {
				if (PL_eval_(c + 1, n) && PL_eval_(c + 2, &n2)) {
					*n *= n2;
					succeed;
				} else
					fail;
			} else if (f == FUN(div, 2) || f == FUN(divide, 2)) {
				if (PL_eval_(c + 1, n) && PL_eval_(c + 2, &n2)) {
					*n /= n2;
					succeed;
				} else
					fail;
			} else if (f == FUN(minus, 1)) {
				if (PL_eval_(c + 1, n)) {
					*n = -*n;
					succeed;
				} else
					fail;
			} else if (f == FUN(_max, 2)) {
				if (PL_eval_(c + 1, n) && PL_eval_(c + 2, &n2)) {
					*n = ((*n > n2) ? *n : n2);
					succeed;
				} else
					fail;
			} else if (f == FUN(_min, 2)) {
				if (PL_eval_(c + 1, n) && PL_eval_(c + 2, &n2)) {
					*n = ((*n < n2) ? *n : n2);
					succeed;
				} else
					fail;
			} else if (f == FUN(_mod, 2)) {
				if (PL_eval_(c + 1, n) && PL_eval_(c + 2, &n2)) {
					*n = *n % n2;
					succeed;
				} else
					fail;
			} else
				PL_warn("Unknow arithmetic operator: %s/%ld", FunName(f), FunArity(f));
		}
	}

	fail;				// Suppress compiler warning
}
