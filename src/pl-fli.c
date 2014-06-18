/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-atom.h"
#include "pl-fun.h"
#include "pl-string.h"
#include "pl-pred.h"
#include "pl-fli.h"
#include "pl-buffer.h"

union cell *PL_new_term_refs(int n)
{
	union cell *r = HP;

	for (; n > 0; n--)
		HP[n].val = __var();

	HP += n;

	return r;
}

union cell *PL_new_term_ref(void)
{
	union cell *r = HP;

	HP->val = __var();
	HP++;

	return r;
}

		 /*******************************
		 *	       CONS-*		*
		 *******************************/
#include <stdarg.h>
void PL_cons_functor(union cell *h, struct functor *fd, ...)
{
	int arity = fd->arity;
	union cell *f;
	va_list args;

	va_start(args, fd);
	h->celp = f = HP;
	HP += (arity + 1);
	f->val = __fun(fd);

	for (f++; f < HP; f++)
		f->celp = deref(va_arg(args, union cell *));

	va_end(args);

}

void PL_cons_list(union cell *l, union cell *head, union cell *tail)
{
	union cell *a;

	l->celp = a = HP;
	HP += 3;

	(a)->val = __cons();
	(a + 1)->celp = deref(head);
	(a + 2)->celp = deref(tail);
}

		 /*******************************
		 *	      GET-*		*
		 *******************************/

static char *malloc_string(char *s)
{
	int n;
	char *m;

	n = strlen(s) + 1;
	m = malloc(n);			// FIXME : check if fail
	memcpy(m, s, n);

	return m;
}

int PL_get_list_codes(union cell *list, const char **s, unsigned flags)
{
	struct ubuffer *b = PL_find_ubs(flags);
	union cell *l;
	static int c;

	l = deref(list);
	while (is_cons(l)) {
		if (!PL_get_intg(l + 1, &c) || c != (c & 0xff))
			goto failed;

		PL_add_ubs(b, c);
		l = deref(l + 2);
	}
	if (!is_nil(l))
		goto failed;

	PL_add_ubs(b, '\0');
	*s = PL_base_ubs(b);
	succeed;

failed:
	PL_lost_ubs(flags);
	fail;
}

int PL_get_list_chars(union cell *list, const char **s, unsigned flags)
{
	struct ubuffer *b = PL_find_ubs(flags);
	union cell *l;

	l = deref(list);
	while (is_cons(l)) {
		const char *a;

		if (!PL_get_atom_chars(l + 1, &a))
			goto failed;

		if (a[0] == '\0' || a[1] != '\0')
			goto failed;

		PL_add_ubs(b, a[0]);
		l = deref(l + 2);
	}
	if (!is_nil(l))
		goto failed;

	PL_add_ubs(b, '\0');
	*s = PL_base_ubs(b);
	succeed;

failed:
	PL_lost_ubs(flags);
	fail;
}

int PL_get_chars(union cell *term, const char **s, unsigned flags)
{
	union cell *t;
	static char tmp[24];		// FIXME : why 24 ??
	char *r;
	int type;

	t = deref(term);

	if ((flags & CVT_ATOM) && is_atom(t)) {
		type = ato_tag;
		r = (char *)get_a_name(t);
	} else if ((flags & CVT_INTEGER) && is_intg(t)) {
		type = int_tag;
		sprintf(tmp, "%ld", (long)get_val(t));
		r = tmp;
	} else if (flags & CVT_LIST)
		return PL_get_list_codes(t, s, flags);
	else if ((flags & CVT_FLOAT) && is_flt(t)) {
		type = flt_tag;
		sprintf(tmp, "%f", get_flt(t));
		r = tmp;
	} else
#if 0					// FIXME : add varName(V). -> static in pl-write.c
	if ((flags & CVT_VARIABLE) && is_var(t)) {
		type = var_tag;
		r = varName(t);
	} else
#endif
		fail;

	if (flags & BUF_MALLOC) {
		*s = malloc_string(r);
	} else if ((flags & BUF_RING && type != ato_tag)) {
		struct ubuffer *b = PL_find_ubs(flags);
		int l = strlen(r) + 1;

		PL_add_x_ubs(b, r, l);
		*s = PL_base_ubs(b);
	} else
		*s = r;

	succeed;
}

int PL_get_name_arity(union cell *t, struct atom ** name, int *arity)
{
	Deref(t);

	if (is_fun(t)) {
		struct functor *f = get_fun(t);

		*name = f->functor;
		*arity = f->arity;
		succeed;
	} else if (is_atom(t)) {
		*name = get_atom(t);
		*arity = 0;
		succeed;
	}

	fail;
}

int PL_get_functor(union cell *t, struct functor ** f)
{
	Deref(t);

	if (is_term(t)) {
		*f = get_fun(t);
		succeed;
	}
// Functor have always arity > 0
//   if ( PL_is_atom(t) )
//   { *f = PL_new_functor(get_atom(t), 0);
//     succeed;
//   }

	fail;
}

int PL_get_arg(int index, union cell *t, union cell *a)
{
	Deref(t);

	if (is_term(t)) {
		int arity = get_arity(t);

		if (index > 0 && index <= arity) {
			mkrefp(a, deref(t + index));
			succeed;
		}
	}

	fail;
}

int PL_get_list(union cell *l, union cell *h, union cell *t)
{
	Deref(l);

	if (is_cons(l)) {
		mkrefp(h, (l + 1));
		mkrefp(t, (l + 2));
		succeed;
	}
	fail;
}

int PL_get_list_(union cell *l, union cell ** h, union cell ** t)
{
	Deref(l);

	if (is_cons(l)) {
		*h = l + 1;
		*t = l + 2;
		succeed;
	}
	fail;
}

int PL_get_head(union cell *l, union cell *h)
{
	Deref(l);

	if (is_list(l)) {
		mkrefp(h, (l + 1));
		succeed;
	}
	fail;
}

int PL_get_tail(union cell *l, union cell *t)
{
	Deref(l);

	if (is_list(l)) {
		mkrefp(t, (l + 2));
		succeed;
	}
	fail;
}

		 /*******************************
		 *             PUT-*  		*
		 *******************************/

void PL_put_functor(union cell *t, struct functor *f)
{
	int arity = f->arity;

	if (arity == 0) {
		PL_put_atom(t, f->functor);
	} else {
		union cell *a = new_struct(f, arity);

		while (arity-- > 0)
			(++a)->val = __var();

		mkrefp(t, a);
	}
}

void PL_put_list(union cell *l)
{
	union cell *a = new_cons();

	a[1].val = a[2].val = __var();
	mkrefp(l, a);
}

		 /*******************************
		 *	       UNIFY		*
		 *******************************/

int PL_unify_atom_chars(union cell *t, const char *chars)
{
	return PL_unify_atom(t, PL_new_atom(chars));
}

int PL_unify_arg(int index, union cell *t, union cell *a)
{
	Deref(t);

	if ((get_tag(t) == fun_tag) && index > 0 && index <= get_arity(t))
		return pl_unify(a, t + index);

	fail;
}

		 /*******************************
		 *	       TYPE		*
		 *******************************/

int PL_term_type(union cell *t)
{
	return Tag(t->val) >> TAG_POS;
}
