/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef PL_FLI_H_
#define PL_FLI_H_

/* pl-fli.c */
cell_t *PL_new_term_refs(int n);
cell_t *PL_new_term_ref(void);
atom_t PL_new_atom(const char *s);
const char *PL_atom_chars(atom_t a);
functor_t PL_new_functor(atom_t f, int a);
atom_t PL_functor_name(functor_t f);
int PL_functor_arity(functor_t f);
void PL_cons_functor(term_t h, functor_t fd, ...);
void PL_cons_list(term_t l, term_t head, term_t tail);
int PL_get_list_codes(term_t list, const char **s, unsigned flags);
int PL_get_list_chars(term_t list, const char **s, unsigned flags);
int PL_get_chars(term_t term, const char **s, unsigned flags);
int PL_get_name_arity(term_t t, atom_t *name, int *arity);
int PL_get_functor(term_t t, functor_t *f);
int PL_get_arg(int index, term_t t, term_t a);
int PL_get_arg_(int index, term_t t, term_t *a);
int PL_get_list(term_t l, term_t h, term_t t);
int PL_get_list_(term_t l, term_t *h, term_t *t);
int PL_get_head(term_t l, term_t h);
int PL_get_tail(term_t l, term_t t);
int PL_get_nil(term_t l);
int PL_is_variable(term_t t);
int PL_is_atom(term_t t);
int PL_is_integer(term_t t);
int PL_is_compound(term_t t);
int PL_is_functor(term_t t, functor_t f);
int PL_is_list(term_t t);
int PL_is_atomic(term_t t);
int PL_is_number(term_t t);
void PL_put_variable(term_t t);
void PL_put_atom(term_t t, atom_t a);
void PL_put_atom_chars(term_t t, const char *s);
void PL_put_integer(term_t t, long i);
void PL_put_functor(term_t t, functor_t f);
void PL_put_list(term_t l);
void PL_put_nil(term_t l);
void PL_put_term(term_t t1, term_t t2);
int PL_unify_atom_chars(term_t t, const char *chars);
int PL_unify_arg(int index, term_t t, term_t a);
int PL_term_type(term_t t);

inline static
void
_PL_get_arg(int index, term_t t, term_t a)
{ cell_t *arg=deref(t)+index;
  mkrefp(a,deref(arg));
}

/* fli.h */
int PL_unify(term_t t1, term_t t2);

/* pl-os.c */
void PL_halt(int status);

/* pl-proc.c */
void *PL_call(term_t closure, int extra, term_t *args);

/* pl-char.c */
atom_t PL_char_to_atom(int c);
term_t PL_mk_code_list(char *s);
term_t PL_mk_char_list(char *s);

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


#endif	// PL_FLI_H_
