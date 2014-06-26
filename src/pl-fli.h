/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef PL_FLI_H_
#define PL_FLI_H_

/* pl-atom.c */
struct atom *PL_new_atom(const char *s);

/* pl-fli.c */
int PL_get_arg(int index, union cell *t, union cell *a);
int PL_get_arg_(int index, union cell *t, union cell **a);
int PL_get_chars(union cell *term, const char **s, unsigned flags);
int PL_get_functor(union cell *t, struct functor **f);
int PL_get_head(union cell *l, union cell *h);
int PL_get_list(union cell *l, union cell *h, union cell *t);
int PL_get_list_(union cell *l, union cell **h, union cell **t);
int PL_get_list_chars(union cell *list, const char **s, unsigned flags);
int PL_get_list_codes(union cell *list, const char **s, unsigned flags);
int PL_get_name_arity(union cell *t, struct atom **name, int *arity);
int PL_get_tail(union cell *l, union cell *t);
int PL_term_type(union cell *t);
int PL_unify_arg(int index, union cell *t, union cell *a);
int PL_unify_atom_chars(union cell *t, const char *chars);
union cell *PL_new_term_ref(void);
union cell *PL_new_term_refs(int n);
void PL_cons_functor(union cell *h, struct functor *fd, ...);
void PL_cons_list(union cell *l, union cell *head, union cell *tail);
void PL_put_functor(union cell *t, struct functor *f);
void PL_put_list(union cell *l);

		 /*******************************
                 *             ATOMS            *
                 *******************************/

inline static const char *PL_atom_chars(struct atom *a)
{
	return a->name;
}

inline static struct atom *PL_functor_name(struct functor *f)
{
	return f->functor;
}

inline static int PL_functor_arity(struct functor *f)
{
	return f->arity;
}

//######################################################################

inline static int PL_is_var(union cell *c)
{
	return __isVar(Val(c));
}

inline static int PL_is_integer(union cell *c)
{
	return __isInteger(Val(c));
}

inline static int PL_is_number(union cell *c)
{
	return __isNumber(Val(c));
}

inline static int PL_is_atom(union cell *c)
{
	return __isAtom(Val(c));
}

inline static int PL_is_atomic(union cell *c)
{
	return __isAtomic(Val(c));
}

inline static int PL_is_term(union cell *c)
{
	return __isTerm(Val(c));
}

inline static int PL_is_struct(union cell *c)
{
	return __isStruct(Val(c));
}

inline static int PL_is_functor(union cell *t, struct functor *f)
{
	return f == get_fun(deref(t));
}

inline static int PL_is_list(union cell *c)
{
	return __isList(Val(c));
}

inline static int PL_is_cons(union cell *c)
{
	return __isCons(Val(c));
}

inline static int PL_get_nil(union cell *c)
{
	return __isNil(Val(c));
}

// The next ones must be deref
inline static int is_cons(union cell *c)
{
	return __isCons(c->val);
}

inline static int is_nil(union cell *c)
{
	return __isNil(c->val);
}

inline static int is_list(union cell *c)
{
	return __isList(c->val);
}

		 /*******************************
                 *              PUT             *
                 *******************************/
inline static void PL_put_var(union cell *t)
{
	mkrefp(t, new_var());
}

inline static void PL_put_integer(union cell *v, long N)
{
	v->val = __intg(N);
	return;
}

inline static void PL_put_float(union cell *v, double N)
{
	v->celp = new_flt(N);
	return;
}

inline static void PL_put_atom(union cell *v, struct atom *A)
{
	v->celp = &(A->atom);
	return;
}

inline static void PL_put_nil(union cell *l)
{
	PL_put_atom(l, ATOM(nil));
}

inline static void PL_put_term(union cell *t1, union cell *t2)
{
	mkrefp(t1, deref(t2));
}

inline static void PL_put_atom_chars(union cell *t, const char *s)
{
	PL_put_atom(t, PL_new_atom(s));
}

//######################################################################

inline static int PL_get_integer(union cell *c, int *n)
{
debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case int_tag:
		*n = get_val(c);
		succeed;
	default:
		fail;
	}
}

#define PL_get_intg(c,n)	PL_get_integer(c,n)

inline static int PL_get_long(union cell *c, long *n)
{
debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case int_tag:
		*n = (long)get_val(c);
		succeed;
	default:
		fail;
	}
}

inline static int PL_get_flt(union cell *c, double *d)
{
debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case flt_tag:
		*d = get_flt(c);
		succeed;
	default:
		fail;
	}
}

inline static int PL_get_pointer(union cell *c, void **ptr)
{
debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case int_tag:
		*ptr = get_addr(c);
		succeed;
	default:
		fail;
	}
}

inline static struct atom *PL_get_atom(union cell *c)
{
debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case ato_tag:
		return (struct atom *) c;
	default:
		fail;
	}
}

inline static int PL_get_atom_chars(union cell *c, const char **s)
{
debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case ato_tag:
		*s = get_a_name(c);
		succeed;
	default:
		fail;
	}
}

inline static int PL_unify_flt(register union cell *c, double d)
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

inline static int PL_unify_long(register union cell *c, long i)
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

inline static int PL_unify_intg(register union cell *c, int i)
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

inline static int PL_unify_bool(register union cell *c, int i)
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

inline static int PL_unify_flag(register union cell *c, int i)
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

inline static int PL_unify_integer(union cell *c, long i)
{
	return PL_unify_intg(c, (int)i);
}

inline static int PL_unify_atom(register union cell *c, struct atom *A)
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

inline static int PL_unify_nil(register union cell *c)
{
debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case var_tag:
		PL_put_atom(c, ATOM(nil));
		trail(c);
		succeed;
	case ato_tag:
		return isatom(ATOM(nil), c);
	default:
		fail;
	}
}

// OK for atom, intg.
inline static int PL_unify_atomic(register union cell *c, union cell at)
{
	c = deref(c);

	if (c->val == at.val)
		succeed;
	else if (is_var(c)) {
		if (at.tag_sval.tag == ato_tag)
			c->val = at.val & VAL_MASK;
		else
			c->val = at.val;
		trail(c);
		succeed;
	} else
		fail;
}

inline static int PL_unify_fun(register union cell *c, struct functor *F)
// PRE : c must be deref.
{
	if (is_var(c)) {
		int n = FunArity(F);
		union cell *f = new_struct(F, n);

		for (; n > 0; n--)
			f[n].val = __var();

		mkrefp(c, f);
		trail(c);
		succeed;
	} else
		return c->val == __fun(F);
}

inline static int PL_unify_functor(union cell *t, struct functor *f)
{
	return PL_unify_fun(deref(t), f);
}

inline static int PL_unify_list(union cell *l, union cell **h, union cell **t)
{
	union cell *c;
debut:
	switch (get_tag(l)) {
	case ref_tag:
		l = l->celp;
		goto debut;
	case var_tag:
		l->celp = c = new_cons();
		trail(l);
		*h = c + 1;
		(c + 1)->val = __var();
		*t = c + 2;
		(c + 2)->val = __var();
		succeed;
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

inline static int PL_unify_key(union cell *c, union cell *key)
{
	switch (get_tag(key)) {
	case ato_tag:
		return PL_unify_atom(c, get_addr(key));
	case int_tag:
		return PL_unify_integer(c, get_val(key));
	case fun_tag:
		return PL_unify_functor(c, get_fun(key));
	default:
		fail;
	}
}

//######################################################################

/* pl-os.c */
void PL_halt(int status);

/* pl-char.c */
struct atom *PL_char_to_atom(int c);
union cell *PL_mk_code_list(char *s);
union cell *PL_mk_char_list(char *s);

// For PL_get_chars
#define	CVT_ATOM	(1<<0)
#define	CVT_INTEGER	(1<<1)
#define	CVT_FLOAT	(1<<2)
#define	CVT_STRING	(1<<3)
#define	CVT_LIST	(1<<4)
#define	CVT_VARIABLE	(1<<5)
#define CVT_NUMBER	(CVT_INTEGER|CVT_FLOAT)
#define CVT_ATOMIC	(CVT_NUMBER|CVT_ATOM|CVT_STRING)
#define CVT_ALL		0xFF

#ifndef BUF_RING
#define BUF_DISCARDABLE 0
#define BUF_MALLOC      (1<<8)
#define BUF_RING        (1<<9)
#endif

#endif
