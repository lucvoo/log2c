/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-pred.h"

int PL_next_goal(void)
{
	return (0);
}

int pl_unify(register union cell * d1, register union cell * d2)
{
	d1 = deref(d1);

debut:
	if (d1 == d2)
		goto OK;		// same object

	switch (get_tag(d2)) {
	case ref_tag:
		d2 = d2->celp;
		goto debut;
	case var_tag:
		if (is_var(d1) && (d1 > d2))	// youngest var
		{
			mkrefp(d1, d2);
			trail(d1);
			goto OK;
		} else {
			mkrefp(d2, d1);
			trail(d2);
			goto OK;
		}

	case ato_tag:
		if (is_var(d1)) {
			mkrefp(d1, d2);
			trail(d1);
			goto OK;
		} else
			goto KO;

	case int_tag:
		if (d1->val == d2->val)
			goto OK;
		if (is_var(d1)) {
			d1->val = d2->val;
			trail(d1);
			goto OK;
		} else
			goto KO;

	case flt_tag:
		if (is_var(d1)) {
			mkrefp(d1, d2);
			trail(d1);
			goto OK;
		} else if (is_flt(d1) && (get_flt(d1) == get_flt(d2)))
			goto OK;
		else
			goto KO;

	case fun_tag:
		if (d1->val == d2->val) {
			if (isfun(FUN(dot, 2), d2)) {
				if (!pl_unify(d1 + 1, d2 + 1))
					goto KO;
				d1 = deref(d1 + 2);
				d2 = d2 + 2;
				goto debut;
			} else {
				int n = get_arity(d2);
				for (; n > 1; n--)
					if (!pl_unify(++d1, ++d2))
						goto KO;

				d1 = deref(d1 + 1);
				d2 = d2 + 1;
				goto debut;
			}
		} else if (is_var(d1)) {
			mkrefp(d1, d2);
			trail(d1);
			goto OK;
		} else
			goto KO;

	}

OK:	return (1);
KO:	return (0);
}

// FIXME : add floating number
int PL_eval_(union cell * c, int *n)
{
	int n2;				//, n2;

debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case var_tag:
		PL_warn("Unbound variable in arithmetic expression");
		return (0);
	case ato_tag:
		PL_warn("Unknow arithmetic operator: %s/%d", AtomName(get_atom(c)), 0);
		return (0);
	case int_tag:
		*n = get_val(c);
		return (1);
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

int PL_can_unify(union cell * a, union cell * b)
{
	int r;
	struct mark m;
	Mark(m);
	r = pl_unify(a, b);
	Undo(m);
	return (r);
}

int PL_not_unify(union cell * a, union cell * b)
{
	return (!PL_can_unify(a, b));
}
