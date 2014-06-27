/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef PL_FLI_H_
#define PL_FLI_H_

/* pl-arith.c */
int PL_eval_(union cell *c, long *n);

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

inline static void PL_put_intg(union cell *v, long N)
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
	v->celp = &(A->cell);
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

inline static int PL_get_intg(union cell *c, int *n)
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

/* pl-unify.c */
int pl_unify(union cell *d1, union cell *d2);
int PL_can_unify(union cell *a, union cell *b);
int PL_not_unify(union cell *a, union cell *b);
int PL_unify_intg(union cell *c, long i);
int PL_unify_flt(union cell *c, double d);
int PL_unify_atom(union cell *c, struct atom *A);
int PL_unify_functor(union cell *c, struct functor *F);
int PL_unify_list(union cell *l, union cell **h, union cell **t);
int PL_unify_bool(union cell *c, int i);
int PL_unify_flag(union cell *c, int i);
int PL_unify_atomic(union cell *c, union cell at);
int PL_unify_key(union cell *c, union cell *key);

inline static int PL_unify_nil(union cell *c)
{
	return PL_unify_atom(c, ATOM(nil));
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
