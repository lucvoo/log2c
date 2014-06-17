/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef PL_FLI_H_
#define PL_FLI_H_

/* pl-atom.c */
atom_t	PL_new_atom(const char *s);

/* pl-fli.c */
int	PL_get_arg(int index, term_t t, term_t a);
int	PL_get_arg_(int index, term_t t, term_t *a);
int	PL_get_chars(term_t term, const char **s, unsigned flags);
int	PL_get_functor(term_t t, functor_t *f);
int	PL_get_head(term_t l, term_t h);
int	PL_get_list(term_t l, term_t h, term_t t);
int	PL_get_list_(term_t l, term_t *h, term_t *t);
int	PL_get_list_chars(term_t list, const char **s, unsigned flags);
int	PL_get_list_codes(term_t list, const char **s, unsigned flags);
int	PL_get_name_arity(term_t t, atom_t *name, int *arity);
int	PL_get_tail(term_t l, term_t t);
int	PL_term_type(term_t t);
int	PL_unify_arg(int index, term_t t, term_t a);
int	PL_unify_atom_chars(term_t t, const char *chars);
term_t  PL_new_term_ref(void);
term_t  PL_new_term_refs(int n);
void	PL_cons_functor(term_t h, functor_t fd, ...);
void	PL_cons_list(term_t l, term_t head, term_t tail);
void	PL_put_functor(term_t t, functor_t f);
void	PL_put_list(term_t l);


                 /*******************************
                 *             ATOMS            *
                 *******************************/

inline static
const char *
PL_atom_chars(atom_t a)
{ return a->name; }

inline static
atom_t
PL_functor_name(functor_t f)
{ return f->functor; }

inline static
int
PL_functor_arity(functor_t f)
{ return f->arity; }

//######################################################################

INLINE_DECL
int PL_is_var(cell_t *c)
{ return(__isVar(Val(c))); }

INLINE_DECL
int PL_is_integer(cell_t *c)
{ return(__isInteger(Val(c))); }

INLINE_DECL
int PL_is_number(cell_t *c)
{ return(__isNumber(Val(c))); }

INLINE_DECL
int PL_is_atom(cell_t *c)
{ return(__isAtom(Val(c))); }

INLINE_DECL
int PL_is_atomic(cell_t *c)
{ return(__isAtomic(Val(c))); }

INLINE_DECL
int PL_is_term(cell_t *c)
{ return(__isTerm(Val(c))); }

INLINE_DECL
int PL_is_struct(cell_t *c)
{ return(__isStruct(Val(c))); }

INLINE_DECL
int
PL_is_functor(term_t t, functor_t f)
{ return(f==get_fun(deref(t))); }

INLINE_DECL
int PL_is_list(cell_t *c)
{ return(__isList(Val(c))); }

INLINE_DECL
int PL_is_cons(cell_t *c)
{ return(__isCons(Val(c))); }

INLINE_DECL
int PL_get_nil(cell_t *c)
{ return(__isNil(Val(c))); }

// The next ones must be deref
INLINE_DECL
int is_cons(cell_t *c)
{ return(__isCons(c->val)); }

INLINE_DECL
int is_nil(cell_t *c)
{ return(__isNil(c->val)); }

INLINE_DECL
int is_list(cell_t *c)
{ return(__isList(c->val)); }


                 /*******************************
                 *              PUT             *
                 *******************************/
inline static
void
PL_put_var(term_t t)
{ mkrefp(t, new_var());
}

inline static
void PL_put_integer(term_t v, long N)
{ v->val=__intg(N);
  return;
}

inline static
void PL_put_float(term_t v, double N)
{ v->celp=new_flt(N);
  return;
}

inline static
void PL_put_atom(term_t v, atom_t A)
{ v->celp=&(A->atom);
  return;
}

inline static
void
PL_put_nil(term_t l)
{ PL_put_atom(l,ATOM(nil));
}

inline static
void
PL_put_term(term_t t1, term_t t2)
{ mkrefp(t1,deref(t2));
}

inline static
void
PL_put_atom_chars(term_t t, const char *s)
{ PL_put_atom(t, PL_new_atom(s));
}

//######################################################################

INLINE_DECL
int PL_get_integer(cell_t *c, int *n)
{ debut:
  switch(get_tag(c))
  { case ref_tag:  c=c->celp;
                   goto debut;
    case int_tag: *n=get_val(c);
		   succeed;
    default:       fail;
  }
}
#define PL_get_intg(c,n)	PL_get_integer(c,n)

INLINE_DECL
int PL_get_long(cell_t *c, long *n)
{ debut:
  switch(get_tag(c))
  { case ref_tag:  c=c->celp;
                   goto debut;
    case int_tag: *n=(long) get_val(c);
		   succeed;
    default:       fail;
  }
}

INLINE_DECL
int PL_get_flt(cell_t *c, double *d)
{ debut:
  switch(get_tag(c))
  { case ref_tag:  c=c->celp;
                   goto debut;
    case flt_tag:  *d=get_flt(c);
		   succeed;
    default:       fail;
  }
}

INLINE_DECL
int PL_get_pointer(cell_t *c, void **ptr)
{ debut:
  switch(get_tag(c))
  { case ref_tag:  c=c->celp;
                   goto debut;
    case int_tag: *ptr=get_addr(c);
		   succeed;
    default:       fail;
  }
}

INLINE_DECL
atom_t PL_get_atom(cell_t *c)
{ debut:
  switch(get_tag(c))
  { case ref_tag:  c=c->celp;
                   goto debut;
    case ato_tag:  return (atom_t) c;
    default:       fail;
  }
}

INLINE_DECL
int PL_get_atom_chars(cell_t *c, const char **s)
{ debut:
  switch(get_tag(c))
  { case ref_tag:  c=c->celp;
                   goto debut;
    case ato_tag: *s=get_a_name(c);
		   succeed;
    default:       fail;
  }
}


INLINE_DECL
int PL_unify_flt(register cell_t *c, double d)
{ debut:
  switch(get_tag(c))
  { case ref_tag:  c=c->celp;
                   goto debut;
    case var_tag:  mkrefp(c,new_flt(d));
                   trail(c);
                   succeed;
    case flt_tag:  return(isflt(d,c));
    default:       fail;
  }
}

INLINE_DECL
int PL_unify_long(register cell_t *c, long i)
{ debut:
  switch(get_tag(c))
  { case ref_tag:  c=c->celp;
                   goto debut;
    case var_tag:  PL_put_integer(c,i);
                   trail(c);
                   succeed;
    case int_tag: return(isintg(i,c));
    default:       fail;
  }
}

INLINE_DECL
int PL_unify_intg(register cell_t *c, int i)
{ debut:
  switch(get_tag(c))
  { case ref_tag:  c=c->celp;
                   goto debut;
    case var_tag:  PL_put_integer(c,i);
                   trail(c);
                   succeed;
    case int_tag: return(isintg(i,c));
    default:       fail;
  }
}

INLINE_DECL
int PL_unify_bool(register cell_t *c, int i)
{ debut:
  switch(get_tag(c))
  { case ref_tag:  c=c->celp;
                   goto debut;
    case var_tag:  if (i)
                     PL_put_atom(c,ATOM(_true));
                   else
                     PL_put_atom(c,ATOM(_false));
                   trail(c);
                   succeed;
    case ato_tag: if (i)
                     return(isatom(ATOM(_true),c) || isatom(ATOM(_on),c));
                   else
                     return(isatom(ATOM(_false),c) || isatom(ATOM(_off),c));
    default:       fail;
  }
}

INLINE_DECL
int PL_unify_flag(register cell_t *c, int i)
{ debut:
  switch(get_tag(c))
  { case ref_tag:  c=c->celp;
                   goto debut;
    case var_tag:  if (i)
                     PL_put_atom(c,ATOM(_on));
                   else
                     PL_put_atom(c,ATOM(_off));
                   trail(c);
                   succeed;
    case ato_tag: if (i)
                     return(isatom(ATOM(_true),c) || isatom(ATOM(_on),c));
                   else
                     return(isatom(ATOM(_false),c) || isatom(ATOM(_off),c));
    default:       fail;
  }
}

INLINE_DECL
int PL_unify_integer(cell_t *c, long i)
{ return(PL_unify_intg(c,(int)i)); }

INLINE_DECL
int PL_unify_atom(register cell_t *c, atom_t A)
{ debut:
  switch(get_tag(c))
  { case ref_tag:  c=c->celp;
                   goto debut;
    case var_tag:  PL_put_atom(c,A);
                   trail(c);
                   succeed;
    case ato_tag: return(isatom(A,c));
    default:       fail;
  }
}

INLINE_DECL
int PL_unify_nil(register cell_t *c)
{ debut:
  switch(get_tag(c))
  { case ref_tag:  c=c->celp;
                   goto debut;
    case var_tag:  PL_put_atom(c,ATOM(nil));
                   trail(c);
                   succeed;
    case ato_tag: return(isatom(ATOM(nil),c));
    default:       fail;
  }
}

// OK for atom, intg.
INLINE_DECL
int PL_unify_atomic(register cell_t *c, cell_t at)
{ c=deref(c);

  if (c->val==at.val)
    succeed;
  else
  if (is_var(c))
  { if (at.tag_val.tag==ato_tag)
      c->val=at.val & VAL_MASK;
    else
      c->val=at.val;
    trail(c);
    succeed;
  }
  else
    fail;
}

INLINE_DECL
int PL_unify_fun(register cell_t *c, fun_t F)
// PRE : c must be deref.
{ if (is_var(c))
  { int n=FunArity(F);
    cell_t *f=new_struct(F,n);

    for (;n>0;n--)
      f[n].val=__var();

    mkrefp(c,f);
    trail(c);
    succeed;
  }
  else
    return(c->val==__fun(F));
}

INLINE_DECL
int PL_unify_functor(term_t t, functor_t f)
{ return(PL_unify_fun(deref(t),f));
}


INLINE_DECL
int PL_unify_list(cell_t *l, cell_t **h, cell_t **t)
{ term_t c;
  debut:
  switch(get_tag(l))
  { case ref_tag:  l=l->celp;
                   goto debut;
    case var_tag:  l->celp=c=new_cons();
                   trail(l);
                   *h=c+1; (c+1)->val=__var();
                   *t=c+2; (c+2)->val=__var();
                   succeed;
    case fun_tag:  if (l->val==__cons())
                   { *h=l+1; *t=l+2; succeed; }
    default:       fail;
  }
}



INLINE_DECL
int PL_unify_key(cell_t *c, cell_t *key)
{ switch(get_tag(key))
  { case ato_tag: return(PL_unify_atom(c,get_addr(key)));
    case int_tag: return(PL_unify_integer(c,get_val(key)));
    case fun_tag: return(PL_unify_functor(c,get_fun(key)));
    default:      fail;
  }
}

//######################################################################

/* pl-os.c */
void PL_halt(int status);

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
