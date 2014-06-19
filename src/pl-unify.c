/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-pred.h"


int pl_unify(union cell *d1, union cell *d2)
{
	d1 = deref(d1);

debut:
	if (d1 == d2)
		succeed;		// same object

	switch (get_tag(d2)) {
	case ref_tag:
		d2 = d2->celp;
		goto debut;
	case var_tag:
		if (is_var(d1) && (d1 > d2)) {	// youngest var
			mkrefp(d1, d2);
			trail(d1);
			succeed;
		} else {
			mkrefp(d2, d1);
			trail(d2);
			succeed;
		}

	case ato_tag:
		if (is_var(d1)) {
			mkrefp(d1, d2);
			trail(d1);
			succeed;
		} else
			fail;

	case int_tag:
		if (d1->val == d2->val)
			succeed;
		if (is_var(d1)) {
			d1->val = d2->val;
			trail(d1);
			succeed;
		} else
			fail;

	case flt_tag:
		if (is_var(d1)) {
			mkrefp(d1, d2);
			trail(d1);
			succeed;
		} else if (is_flt(d1) && (get_flt(d1) == get_flt(d2)))
			succeed;
		else
			fail;

	case fun_tag:
		if (d1->val == d2->val) {
			int n = get_arity(d2);
			for (; n > 1; n--)
				if (!pl_unify(++d1, ++d2))
					fail;

			d1 = deref(d1 + 1);
			d2 = d2 + 1;
			goto debut;
		} else if (is_var(d1)) {
			mkrefp(d1, d2);
			trail(d1);
			succeed;
		} else
			fail;

	default:
		fail;
	}
}

int PL_can_unify(union cell *a, union cell *b)
{
	int r;
	struct mark m;
	Mark(m);
	r = pl_unify(a, b);
	Undo(m);
	return r;
}

int PL_not_unify(union cell *a, union cell *b)
{
	return !PL_can_unify(a, b);
}

/************************************************************************/

int PL_unify_intg(union cell *c, long i)
{
debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case var_tag:
		PL_put_integer(c, i);
		trail(c);
		succeed;
	case int_tag:
		return isintg(i, c);
	default:
		fail;
	}
}

int PL_unify_flt(union cell *c, double d)
{
debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case var_tag:
		mkrefp(c, new_flt(d));
		trail(c);
		succeed;
	case flt_tag:
		return isflt(d, c);
	default:
		fail;
	}
}

int PL_unify_atom(union cell *c, struct atom *A)
{
debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case var_tag:
		PL_put_atom(c, A);
		trail(c);
		succeed;
	case ato_tag:
		return isatom(A, c);
	default:
		fail;
	}
}

// OK for atom, intg.
// KO for float
int PL_unify_atomic(union cell *c, union cell at)
{
debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case var_tag:
		if (at.tag_sval.tag == ato_tag)
			c->val = at.val & VAL_MASK;
		else
			c->val = at.val;
		trail(c);
		succeed;
	case ato_tag:
	case int_tag:
	case flt_tag:
		if (c->val == at.val)
			succeed;
	default:
		fail;
	}
}

int PL_unify_functor(union cell *c, struct functor *F)
{
debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case var_tag: {
		int n = FunArity(F);
		union cell *f = new_struct(F, n);

		for (; n > 0; n--)
			f[n].val = __var();

		mkrefp(c, f);
		trail(c);
		succeed;
	}
	default:
		return c->val == __fun(F);
	}
}

int PL_unify_list(union cell *l, union cell **h, union cell **t)
{
debut:
	switch (get_tag(l)) {
	case ref_tag:
		l = l->celp;
		goto debut;
	case var_tag: {
		union cell *c;

		l->celp = c = new_cons();
		trail(l);
		(*h = c + 1)->val = __var();
		(*t = c + 2)->val = __var();
		succeed;
	}
	case fun_tag:
		if (l->val == __cons()) {
			*h = l + 1;
			*t = l + 2;
			succeed;
		}
	default:
		fail;
	}
}

/************************************************************************/

int PL_unify_bool(union cell *c, int i)
{
debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case var_tag:
		if (i)
			PL_put_atom(c, ATOM(_true));
		else
			PL_put_atom(c, ATOM(_false));
		trail(c);
		succeed;
	case ato_tag:
		if (i)
			return isatom(ATOM(_true), c) || isatom(ATOM(_on), c);
		else
			return isatom(ATOM(_false), c) || isatom(ATOM(_off), c);
	default:
		fail;
	}
}

int PL_unify_flag(union cell *c, int i)
{
debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case var_tag:
		if (i)
			PL_put_atom(c, ATOM(_on));
		else
			PL_put_atom(c, ATOM(_off));
		trail(c);
		succeed;
	case ato_tag:
		if (i)
			return isatom(ATOM(_true), c) || isatom(ATOM(_on), c);
		else
			return isatom(ATOM(_false), c) || isatom(ATOM(_off), c);
	default:
		fail;
	}
}

int PL_unify_key(union cell *c, union cell *key)
{
	switch (get_tag(key)) {
	case ato_tag:
		return PL_unify_atom(c, get_addr(key));
	case int_tag:
		return PL_unify_intg(c, get_val(key));
	case fun_tag:
		return PL_unify_functor(c, get_fun(key));
	default:
		fail;
	}
}
