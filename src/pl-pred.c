/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-pred.h"


int pl_unify(register union cell *d1, register union cell *d2)
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

OK:	return 1;
KO:	return 0;
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
