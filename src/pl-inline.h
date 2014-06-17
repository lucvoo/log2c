/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef	PL_INLINE_H_
#define PL_INLINE_H_

#include "pl.h"

#define ATOM(A)		(&ATOM_ ## A)
#define FUN_(F)		(&FUN_ ## F)
#define FUN(F,A)	(&FUN_ ## F ## _ ## A)

#define Round(i,align)	((i+align-1) & (-align) )

#define Align_N(ptr,align)	((void *) Round((uintptr_t) ptr,align) )
#define Align(ptr)		Align_N(ptr,CELL_SIZE)

#define Adjust_N(i,align)	((i+align-1) / align )
#define Adjust(i)		Adjust_N(i,CELL_SIZE)

#define get_val(c)	((c)->tag_sval.val)
#define get_uval(c)	((c)->tag_uval.uval)
#define get_addr(c)	((void *) (unsigned long) ((c)->tag_uval.uval))
#define get_tag(c)	((c)->tag_sval.tag)

#define get_fun(c)	((struct functor *) get_addr(c))
#define get_atom(c)	((struct atom *) (c))
#define get_str(c)	((char *) get_addr(c))
#define get_arity(c)	(get_fun(c)->arity)
#define get_a_name(c)	(get_atom(c)->name)
#define get_f_name(c)	(get_fun(c)->name)
#define get_arity(c)	(get_fun(c)->arity)
#define get_intg(c)	get_val(c)
#define get_flt(c)	(*((double *) &(c)[1]))

#define AtomName(A)	((A)->name)
#define FunName(f)	(((f)->functor)->name)
#define FunArity(f)	((f)->arity)

#define	MK_TAG(T)	(((unsigned long)(T))<<TAG_POS)
#define MK_CELL(T,V)	(MK_TAG(T)+(pl_word_t) (V))
#define new_atom(A)	(&((A)->atom))
#define __cons()	__fun(FUN(dot,2))
#define __nil()		__atom(ATOM(nil))

inline static pl_word_t __intg(intg_t N)
{
	return (MK_CELL(int_tag, VAL_MASK & N));
}

inline static pl_word_t __fun(struct functor *F)
{
	return (MK_CELL(fun_tag, F));
}

inline static pl_word_t __var(void)
{
	return (MK_CELL(var_tag, 0));
}

inline static pl_word_t __atom(struct atom *A)
{
	return ((pl_word_t) new_atom(A));
}

inline static int is_ref(union cell * c)
{
	return (get_tag(c) == ref_tag);
}

inline static int is_var(union cell * c)
{
	return (c->val == __var());
}

inline static int is_atom(union cell * c)
{
	return (get_tag(c) == ato_tag);
}

inline static int is_intg(union cell * c)
{
	return (get_tag(c) == int_tag);
}

inline static int is_flt(union cell * c)
{
	return (get_tag(c) == flt_tag);
}

inline static int is_number(union cell * c)
{
	int tag = get_tag(c);
	return (tag == int_tag || tag == flt_tag);
}

inline static int is_atomic(union cell * c)
{
	int tag = get_tag(c);
	return (tag == ato_tag || tag == int_tag || tag == flt_tag);
}

inline static int is_fun(union cell * c)
{
	return (get_tag(c) == fun_tag);
}

inline static int is_term(union cell * c)
{
	return (is_fun(c));
}

inline static union cell * new_intg(long N)
{
	HP->val = __intg(N);
	return (HP++);
}

union cell *new_flt(double r);

inline static union cell * new_var(void)
{
	HP->val = __var();
	return (HP++);
}

inline static union cell * new_void(void)
{
	return (new_var());
}					// FIXME : put void var in local stack

inline static union cell * new_struct(struct functor *F, int N)
{
	register typeof(HP) old_HP;

	old_HP = HP;
	HP->val = __fun(F);
	HP += (N + 1);

	return (old_HP);
}

inline static union cell * new_cons(void)
{
	return (new_struct(FUN(dot, 2), 2));
}

inline static int isatom(struct atom *A, union cell * addr)
{
	return (addr == &(A->atom));	// atoms are unique !
}

inline static int isintg(long N, union cell * addr)
{
	return (addr->val == __intg(N));
}

inline static int isflt(double r, union cell * addr)
{
	return (get_flt(addr) == r);
}

inline static int isfun(struct functor *F, union cell * addr)
{
	return (addr->val == __fun(F));
}

union cell *deref_dbg(union cell * addr);

inline static union cell * deref(union cell * addr)
#if 1
{
	union cell *p = addr;

	while (p->tag_sval.tag == ref_tag) {
		p = p->celp;
	}

	return (p);
}
#else
{
	return deref_dbg(addr);
}
#endif

#define Deref(addr)	while (get_tag(addr)==ref_tag) addr=addr->celp

inline static void mkref(union cell * v, union cell c)
{
	*v = c;

	return;
}

inline static void mkrefp(union cell * v, union cell * c)
{
	v->celp = c;

	return;
}

inline static void trail(union cell * addr)
{
	if (addr < (BTP + 3)->celp)
		*(TP++) = addr;

	return;
}

inline static void reset(register union cell ** a1)
{
	register union cell **tp;

	for (tp = TP; a1 < tp;)
		(*(--tp))->val = MK_TAG(var_tag);

	TP = a1;
	return;
}

inline static void backtrack(void)
{
	FP = BTP;
	HP = (FP + 3)->celp;
	reset(FP[2].tr);
	goto *((FP + 4)->cod);
}

inline static unsigned long round_to_power(unsigned long n)
{
	int r = 1;
	n = n - 1;

	do {
		n /= 2;
		r = r * 2;
	}
	while (n != 0);

	return (r);
}

// 0 and -1 are never a valid value
inline static hash_t SimpleHashValue(union cell * key)
{
debut:
	switch (get_tag(key))		// get the hash value if the key is OK
	{
	case ref_tag:
		key = key->celp;
		goto debut;
	case ato_tag:
	case fun_tag:
		return ((key->val) >> GC_BITS);
	case int_tag:
		return (key->val);
	default:
		return (0);
	}
}

#endif
