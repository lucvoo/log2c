/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-pred.h"
#include "pl-prims.h"
#include "pl-fli.h"
#include "pl-atom.h"
#include "pl-fun.h"
#include "pl-string.h"
#include "pl-buffer.h"
#include "pl-hash.h"
#include <stdlib.h>			// for qsort()

void PL_halt(int status)
{					// FIXME : do I/O clean-up
	//         and ``on_halt'' predicate.
	exit(status);
}

int pl_between(union cell *low, union cell *high, union cell *n, enum control *ctrl)
{
	int l, h, i;
	int *ctxt;

	switch (GetCtrl(ctrl)) {
	case FIRST_CALL:
		if (!PL_get_integer(low, &l) || !PL_get_integer(high, &h))
			PL_warning("between/3: instantiation fault");
		if (PL_get_integer(n, &i)) {
			try((l <= i && i <= h));
			succeed;
		} else if (!PL_is_var(n))
			PL_warning("between/3: instantiation fault");

		try((l <= h));
		PL_unify_intg(n, l);
		if (l == h)
			succeed;
		ctxt = AllocCtxt(int);
		*ctxt = l;
		PL_retry(ctxt);

	case NEXT_CALL:
		ctxt = GetCtxt(ctrl);
		i = *ctxt + 1;		// n is variable
		PL_put_integer(n, i);
		trail(n);
		PL_get_integer(high, &h);
		if (i == h)
			succeed;
		*ctxt = i;
		retry;
	default:
		succeed;
	}
}

int pl_plus(union cell *d1, union cell *d2, union cell *d3)
{
	long a, b, c;

	if (PL_get_long(d1, &a)) {
		if (PL_get_long(d2, &b))
			return PL_unify_intg(d3, a + b);
		else if (PL_get_long(d3, &c))
			return PL_unify_intg(d2, c - a);
	} else				// d1 not intg
	if (PL_get_long(d2, &b)) {
		if (PL_get_long(d3, &c))
			return PL_unify_intg(d1, c - b);
	}

	PL_warning("plus/3 : instantiation fault");
}

int pl_ground(union cell *d)
{
debut:
	switch (get_tag(d)) {
	case ref_tag:
		d = d->celp;
		goto debut;
	case var_tag:
		return 0;
	case ato_tag:
	case int_tag:
	case flt_tag:
		return 1;
	case fun_tag:{
			int n = get_arity(d);
			for (; n > 1; n--)
				if (!pl_ground(++d))
					return 0;

			// if (n==0) return(1);     // nulladic functor
			d = deref(d + 1);
			goto debut;
		}
	}

	return 0;			// Suppress warning
}

int pl_unground(union cell *d)
{
	return !pl_ground(d);
}

static int term_variables(union cell *c, int n)
{
debut:

	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;

	case var_tag:{
			union cell *l = HP;
			int i;

			for (i = n; i > 0; i--, l += 2)
				if (c == (l + 1)->celp)
					goto end;

			(l)->val = __cons();
			(l + 1)->celp = c;
			return n + 1;
		}
	case ato_tag:
	case flt_tag:
	case int_tag:
		break;
	case fun_tag:{
			int i = get_arity(c);

			for (; i > 1; i--)
				n = term_variables(++c, n);

			c++;
			goto debut;
		}
	}
end:
	return n;
}

int pl_term_variables(union cell *t, union cell *fv)
{
	int n;
	union cell *l = HP;

	n = term_variables(t, 0);
	HP += (2 * n + 1);
	(l + 2 * n)->val = __atom(ATOM(nil));

	return pl_unify(fv, l);
}

static int PL_unify_list_chars(union cell *l, const char *s)
{
	for (; *s; s++) {
debut:
		switch (get_tag(l)) {
		case ref_tag:
			l = l->celp;
			goto debut;
		case var_tag:
			goto write;
		case fun_tag:
			if (is_cons(l) && PL_unify_atom(l + 1, PL_char_to_atom(*s))
				) {
				l = l + 2;
				break;
			}
		default:
			fail;
		}
	}
	return PL_unify_nil(l);

write:
	{
		union cell *t = l;
		trail(t);
		for (; *s; s++) {
			t->celp = HP;
			HP[0].val = __cons();
			HP[1].val = __atom(PL_char_to_atom(*s));
			t = HP + 2;
			HP += 3;
		}
		t->val = __nil();
		succeed;
	}
}

int PL_unify_list_codes(union cell *l, const char *s)
{
	for (; *s; s++) {
debut:
		switch (get_tag(l)) {
		case ref_tag:
			l = l->celp;
			goto debut;
		case var_tag:
			goto write;
		case fun_tag:
			if (is_cons(l) && PL_unify_intg(l + 1, *s)) {
				l = l + 2;
				break;
			}
		default:
			fail;
		}
	}
	return PL_unify_nil(l);

write:
	{
		union cell *t = l;
		trail(t);
		for (; *s; s++) {
			t->celp = HP;
			HP[0].val = __cons();
			HP[1].val = __intg((unsigned)*s);
			t = HP + 2;
			HP += 3;
		}
		t->val = __nil();
		succeed;
	}
}

/***************************************************/
/* Atom conversion                                 */
/***************************************************/

// FIXME : add pl_name, pl_number_chars.

// ISO compliant one, not SWI one who is ISO's atom_codes/2
int pl_atom_chars(union cell *a, union cell *list)
{
	const char *s;

	if (PL_get_atom_chars(a, &s))
		return PL_unify_list_chars(list, s);
	else if (PL_get_list_chars(list, &s, BUF_DISCARDABLE)) {
		struct atom *tmp = PL_new_atom(s);
		return PL_unify_atom(a, tmp);
	} else
		fail;
}

int pl_atom_codes(union cell *a, union cell *list)
{
	const char *s;

	if (PL_get_atom_chars(a, &s))
		return PL_unify_list_codes(list, s);
	else if (PL_get_list_codes(list, &s, BUF_DISCARDABLE)) {
		struct atom *tmp = PL_new_atom(s);
		return PL_unify_atom(a, tmp);
	} else
		fail;
}

int pl_atom_char(union cell *c, union cell *ascii)
{
	const char *s;
	int n;

	if (PL_get_atom_chars(c, &s)) {
		if (s[0] != '\0' && s[1] == '\0')
			return PL_unify_intg(ascii, s[0]);
		else
			PL_warning("atom_char/2: not a single char atom");
	} else if (PL_get_integer(ascii, &n) && 0 < n && n < 256) {
		if (n > 0 && n < 256)
			return PL_unify_atom(c, PL_char_to_atom(n));
		else
			PL_warning("atom_char/2: domain error");
	} else
		PL_warning("atom_char/2: instantiation fault");
}

int pl_atom_length(union cell *atom, union cell *len)
{
	const char *s;

	if (PL_get_chars(atom, &s, CVT_ALL))
		return PL_unify_intg(len, strlen(s));

	PL_warning("atom_length/2: instantiation fault");
}

int pl_atom_prefix(union cell *atom, union cell *prefix)
{
	const char *a, *p;

	if (PL_get_chars(atom, &a, CVT_ATOMIC | BUF_RING) &
	    PL_get_chars(prefix, &p, CVT_ATOMIC | BUF_RING))
		return isPrefix(p, a);

	PL_warning("atom_prefix/2: instantiation fault");
}

int pl_functor(union cell *t, union cell *f, union cell *a)
{
	int arity;
	struct atom *name;

	if (PL_get_name_arity(t, &name, &arity))
		return PL_unify_atom(f, name) && PL_unify_intg(a, arity);

	if (PL_is_atomic(t))
		return pl_unify(f, t) && PL_unify_intg(a, 0);

	try(PL_get_integer(a, &arity));

	if (arity == 0 && PL_is_atomic(f))
		return pl_unify(t, f);
	else if (arity < 0)
		PL_warning("functor/3: illegal arity");
	else if ((name = PL_get_atom(f)))
		return PL_unify_functor(t, PL_new_functor(name, arity));
	else
		fail;
}

int pl_univ(union cell *t, union cell *l)
{
	int arity;
	struct atom *name;
	union cell *h;

	t = deref(t);
	if (is_fun(t)) {
		int n;
		struct functor *f = get_fun(t);
		name = f->functor;
		arity = f->arity;

		try(PL_unify_list(l, &h, &l) && PL_unify_atom(h, name));
		for (n = 1; n <= arity; n++) {
debut:
			switch (get_tag(l)) {
			case ref_tag:
				l = l->celp;
				goto debut;
			case var_tag:
				l->celp = HP;
				trail(l);
				{
					n = n - (arity + 1);
					t = t + (arity + 1);
					HP = HP - 2 * n;
					while (n < 0) {
						HP[2 * n].val = __cons();
						HP[2 * n + 1].celp = t + n;
						n++;
					}
					HP[0].val = __nil();
					HP++;
					succeed;
				}
			case fun_tag:
				if (is_cons(l) && pl_unify(t + n, l + 1)) {
					l = l + 2;
					break;
				} else
					fail;
			default:
				fail;
			}
		}
		return PL_unify_nil(l);
	} else if (is_atomic(t))
		return PL_unify_list(l, &h, &l) && PL_unify_nil(l) && pl_unify(t, h);
	else if (is_var(t)) {
		union cell *term;
		try(PL_unify_list(l, &h, &l));
		l = deref(l);
		if (is_nil(l))
			return pl_unify(t, h);
		h = deref(h);
		try(is_atom(h));

		term = HP++;		// for functor
		while (is_cons(l)) {
			HP->celp = l + 1;
			HP++;
			l = deref(l + 2);
		}
		try(is_nil(l));
		arity = (HP - term) - 1;
		term->val = __fun(PL_new_functor(get_atom(h), arity));
		mkrefp(t, term);
		trail(t);
		succeed;
	} else
		fail;
}

inline static int cmp_flt(double d1, double d2)
{
	return (d1 < d2) ? -1 : ((d1 == d2) ? 0 : 1);
}

inline static int cmp_addr(void *a1, void *a2)
{
	return (a1 < a2) ? -1 : ((a1 == a2) ? 0 : 1);
}

inline static int cmp_intg(int i1, int i2)
{
	return (i1 < i2) ? -1 : ((i1 == i2) ? 0 : 1);
}

int pl_std_cmp(union cell *t1, union cell *t2)
{
loop:
	Deref(t2);
	if (t1 == t2)
		return 0;

	switch (get_tag(t1)) {
	case ref_tag:
		t1 = t1->celp;
		goto loop;
	case var_tag:
		if (is_var(t2))
			return cmp_addr(t1, t2);
		break;
	case ato_tag:
		if (is_var(t2))
			return 1;
		if (is_atom(t2))
			return strcmp(get_a_name(t1), get_a_name(t2));
		break;
	case int_tag:
		if (get_tag(t2) < int_tag)
			return 1;
		if (is_intg(t2))
			return cmp_intg(get_intg(t1), get_intg(t2));
		if (is_flt(t2))
			return cmp_flt((double)get_intg(t1), get_flt(t2));
		break;
	case flt_tag:
		if (get_tag(t2) < int_tag)
			return 1;
		if (is_intg(t2))
			return cmp_flt(get_flt(t1), (double)get_intg(t2));
		if (is_flt(t2))
			return cmp_flt(get_flt(t1), get_flt(t2));
		break;
	case fun_tag:
		if (get_tag(t2) < fun_tag)
			return 1;
		if (is_fun(t2)) {
			struct functor *f1 = get_fun(t1);
			struct functor *f2 = get_fun(t2);
			int r, n;

			r = strcmp(FunName(f1), FunName(f2));
			if (r)
				return r;
			r = cmp_intg(n = FunArity(f1), FunArity(f2));
			if (r)
				return r;

			for (; n > 1; n--)
				if ((r = pl_std_cmp(++t1, ++t2)))
					return r;

			t2++;
			t1++;
			goto loop;
		}
		break;
	}
	return -1;
}

int pl_std_eq(union cell *t1, union cell *t2)
{
loop:
	t2 = deref(t2);
	if (t1 == t2)
		succeed;

	switch (get_tag(t1)) {
	case ref_tag:
		t1 = t1->celp;
		goto loop;
	case var_tag:
		fail;
	case ato_tag:
		fail;
	case int_tag:
		return t1->val == t2->val;
	case flt_tag:
		if (is_flt(t2) && (get_flt(t1) == get_flt(t2)))
			succeed;
		else
			fail;
	case fun_tag:
		if (t1->val == t2->val) {
			int n = get_arity(t1);

			for (; n > 1; n--)
				if (!pl_std_eq(++t1, ++t2))
					fail;

			t2++;
			t1++;
			goto loop;
		}
	}

	fail;				// Suppress compiler warning
}

static int struct_eq(union cell *t1, union cell *t2)
{
	Deref(t1);
	Deref(t2);

loop:
	switch (get_tag(t1)) {
	case ref_tag:
		fail;			// impossible error occur.
	case var_tag:
		if (!is_var(t2))
			fail;
		if (t1->val == __var()) {
			if (t2->val != __var())
				fail;
			t1->val = t2->val = (__var() + (unsigned long)t1);
			HP[0].celp = t1;
			HP[1].celp = t2;
			HP += 2;
		} else if (t2->val != t1->val)
			fail;
		break;
	case ato_tag:
		if (t1 != t2)
			fail;
		break;
	case flt_tag:
		if (!is_flt(t2) || (get_flt(t1) != get_flt(t2)))
			fail;
		break;
	case int_tag:
		if (t1->val != t2->val)
			fail;
		break;
	case fun_tag:
		if (t1->val != t2->val)
			fail;
		else {
			int n = get_arity(t1);
			for (; n > 1; n--)
				if (!struct_eq(++t1, ++t2))
					fail;

			t1 = deref(t1 + 1);
			t2 = deref(t2 + 1);
			goto loop;
		}
		break;
	}
	succeed;
}

int pl_struct_eq(union cell *t1, union cell *t2)
{
	union cell *old_HP = HP;
	int rval;

	rval = struct_eq(t1, t2);
	for (; HP != old_HP;)
		(--HP)->celp->val = __var();

	return rval;
}

int pl_concat(union cell *a1, union cell *a2, union cell *a3)
{
	const char *s1 = 0, *s2 = 0, *s3 = 0;
	int l1, l2, l3;
	char *tmp;

	PL_get_chars(a1, &s1, CVT_ATOMIC | BUF_RING);
	PL_get_chars(a2, &s2, CVT_ATOMIC | BUF_RING);
	PL_get_chars(a3, &s3, CVT_ATOMIC | BUF_RING);

	if (s1 && s2) {
		l1 = strlen(s1);
		l2 = strlen(s2);
		tmp = alloca(l1 + l2 + 1);
		strcpy(tmp, s1);
		strcpy(tmp + l1, s2);
		return PL_unify_atom_chars(a3, tmp);
	}

	if (!s3)
		PL_warning("concat/3: instantiation fault(1)");

	if (s1) {
		if (isPrefix(s1, s3))
			return PL_unify_atom_chars(a2, s3 + strlen(s1));
		else
			fail;
	}

	if (s2) {
		int ld;			/* fixed 13/09/93 for: */
		char *q;		/* concat(X, ' ', 'xxx  ') */

		l2 = strlen(s2);
		l3 = strlen(s3);
		ld = l3 - l2;
		if (l2 > l3 || !streq(s3 + ld, s2))
			fail;
		// q = alloca(ld+1);
		// strncpy(q, s3, ld);
		{
			q = (char *)SHP;
			SHP += (ld + CELL_SIZE) / CELL_SIZE;
			strncpy(q, s3, ld);
			SHP = (typeof(SHP)) q;
		}
		q[ld] = '\0';
		return PL_unify_atom_chars(a1, q);
	}

	PL_warning("concat/3: instantiation fault(2)");
}

__inline__ int pl_concat_atom3(union cell *list, union cell *sep, union cell *atom)
{
	struct ubuffer *b;
	const char *sp, *s;
	int splen;
	union cell *l = deref(list);

	if (sep) {
		if (!PL_get_chars(sep, &sp, CVT_ATOMIC | BUF_RING))
			PL_warning("concat_atom/3: illegal separator");
		splen = strlen(sp);
	} else
		splen = 0;

	b = PL_find_ubs(BUF_DISCARDABLE);

	if (sep)			// add the first element
	{
		if (is_cons(l) && PL_get_chars(l + 1, &s, CVT_ATOMIC)) {
			PL_add_x_ubs(b, s, strlen(s));
			l = deref(l + 2);
		}
	}
	// add remaining elements
	while (is_cons(l) && PL_get_chars(l + 1, &s, CVT_ATOMIC)) {
		if (sep)		// with separator if needed
			PL_add_x_ubs(b, sp, splen);
		PL_add_x_ubs(b, s, strlen(s));
		l = deref(l + 2);
	}

	if (is_nil(l)) {
		struct atom *a;

		PL_add_ubs(b, '\0');
		a = PL_new_atom(PL_base_ubs(b));
		return PL_unify_atom(atom, a);
	} else {
		if (sep)
			PL_warning("concat_atom/3: instantiation fault");
		else
			PL_warning("concat_atom/2: instantiation fault");
	}
}

int pl_concat_atom2(union cell *list, union cell *atom)
{
	return pl_concat_atom3(list, 0, atom);
}

int pl_halt(union cell *stat)
{
	int n;

	if (!PL_get_integer(stat, &n))
		n = 1;

	PL_halt(n);
	fail;
}

int pl_arg(union cell *n, union cell *term, union cell *arg)
{
	int idx;

#ifndef	FAST_PL_ARG
	struct atom *name;
	int arity;

	if (!PL_get_name_arity(term, &name, &arity))
		PL_warning("arg/3: second argument in not a term");

	if (PL_get_integer(n, &idx) && idx > 0 && idx <= arity)
#else
	if (PL_get_integer(n, &idx) && idx > 0)
#endif
		return pl_unify(arg, deref(term) + idx);
	else
		fail;
}

int pl_setarg(union cell *n, union cell *term, union cell *value)
{
	int arity, argn;
	struct atom *name;

	if (!PL_get_integer(n, &argn) || !PL_get_name_arity(term, &name, &arity))
		PL_warning("setarg/3: instantiation fault");

	if (argn < 1 || argn > arity)
		fail;

	term = deref(term);
	if (term < H_STK || term > SH_STK)
		PL_warning("setarg/3: attempt to assign to a static term");
	term[argn].celp = value;
	succeed;
}

int pl_hpjw(union cell *str, union cell *h_val)
{
	hash_t h;
	const char *s;

	if (!PL_get_atom_chars(str, &s))
		fail;

	h = PL_hpjw(s);

	return PL_unify_intg(h_val, h);
}

static int PL_HashTerm(union cell *term, hash_t *hval)
{
	while (1)
		switch (get_tag(term)) {
		case ref_tag:
			term = deref(term);
			continue;
		case var_tag:
			fail;
		case ato_tag:
			*hval = get_atom(term)->hash;
			succeed;
		case int_tag:
			*hval = get_val(term);
			succeed;
		case flt_tag:{
				union {
					double f;
					long l[sizeof(double) / sizeof(long)];
				} flt;
				flt.f = get_flt(term);
				if (sizeof(double) == 2 * sizeof(long))
					*hval = flt.l[0] ^ flt.l[1];
				else if (sizeof(double) == sizeof(long))
					*hval = flt.l[0];
				succeed;
			}
		case fun_tag:{
				hash_t h, val;
				struct functor *f = get_fun(term);
				int arity = f->arity;

				val = f->functor->hash + arity;

				for (; arity > 0; arity--) {
					if (!PL_HashTerm(++term, &h))
						fail;
					else
						val += h;
				}
				*hval = val;
				succeed;
			}
		default:
			fail;
		}
}

int pl_hash_term(union cell *term, union cell *hash)
{
	hash_t h;

	if (PL_HashTerm(term, &h))
		return PL_unify_intg(hash, h);
	else
		fail;
}

int pl_repeat(enum control *ctrl)
{
	retry;
}

static int NumberVars(union cell *c, struct functor *functor, int start)
{
	while (1)
		switch (get_tag(c)) {
		case ref_tag:
			c = c->celp;
			continue;
		case var_tag:{
				union cell *val = new_struct(functor, 1);
				val[1].val = __intg(start++);
				mkrefp(c, val);
				trail(c);
				return start;
			}
		case fun_tag:{
				int a = get_arity(c);
				for (; a > 1; a--)
					start = NumberVars(++c, functor, start);

				c++;
				continue;
			}
		default:
			return start;
		}
}

int pl_numbervars(union cell *term, union cell *functor, union cell *start, union cell *end)
{
	struct atom *fun;
	struct functor *f;
	int n;

	if ((fun = PL_get_atom(functor)) && PL_get_intg(start, &n)) {
		f = PL_new_functor(fun, 1);
		n = NumberVars(term, f, n);
		return PL_unify_intg(end, n);
	} else
		PL_warning("numbervars/4: instantiation fault");
}
