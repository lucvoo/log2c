/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	_FLI_H_
#define _FLI_H_

#include "pl.h"
#include "pl-inline.h"
#include <stdio.h>
#include <string.h>


#define PL_REFERENCE	(ref_tag)
#define PL_VARIABLE	(var_tag)
#define PL_ATOM		(ato_tag)
#define PL_INTEGER	(int_tag<<29)
#define PL_FLOAT	(flt_tag<<29)
#define PL_TERM		(fun_tag<<29)


#define succeed	return(SUCCEED)
#define fail	return(FAIL)
#define retry	return(RETRY)
#define try(G)	if (!(G)) fail; else 


#define Tag(v)	(v & TAG_MASK)
#define Val(c)	({ cell_t d=*c; while (Tag(d.val)==(ref_tag<<29)) { d=*(d.celp); }; d.val; })

#define	__isVar(v)	(v==(var_tag<<29))
#define __isAtom(v) 	(Tag(v)==(ato_tag<<29))
#define __isInteger(v) 	(Tag(v)==(int_tag<<29))
#define __isFloat(v) 	(Tag(v)==(flt_tag<<29))
#define __isTerm(v) 	(Tag(v)==(fun_tag<<29))
#define	__isCons(v)	(v==__fun(FUN(dot,2)))
#define	__isNil(v)	(v==((ato_tag<<29) + (uint) ATOM(nil)))

#define __isNumber(V)	({ uint v=V; int r=(__isInteger(v) || __isFloat(v)); r; })
#define	__isStruct(V)	({ uint v=V; int r=(v!=__fun(FUN(dot,2)) && __isTerm(v)); r; })
#define __isAtomic(V)	({ uint v=V; int r=(!__isVar(v) && !__isTerm(v)); r; })
#define __isList(V)	({ uint v=V; int r=(__isCons(v) || __isNil(v)); r; })

//////////////////////
INLINE_DECL
int isVar(cell_t *c)
{ return(__isVar(Val(c))); }

INLINE_DECL
int isAtom(cell_t *c)
{ return(__isAtom(Val(c))); }

INLINE_DECL
int isInteger(cell_t *c)
{ return(__isInteger(Val(c))); }

INLINE_DECL
int isNumber(cell_t *c)
{ return(__isNumber(Val(c))); }

INLINE_DECL
int isTerm(cell_t *c)
{ return(__isTerm(Val(c))); }

INLINE_DECL
int isStruct(cell_t *c)
{ return(__isStruct(Val(c))); }

INLINE_DECL
int isAtomic(cell_t *c)
{ return(__isAtomic(Val(c))); }

INLINE_DECL
int isList(cell_t *c)
{ return(__isList(Val(c))); }

INLINE_DECL
int isCons(cell_t *c)
{ return(__isCons(Val(c))); }

INLINE_DECL
int isNil(cell_t *c)
{ return(__isNil(Val(c))); }

// The three next must be deref
INLINE_DECL
int is_cons(cell_t *c)
{ return(__isCons(c->val)); }

INLINE_DECL
int is_nil(cell_t *c)
{ return(__isNil(c->val)); }

INLINE_DECL
int is_list(cell_t *c)
{ return(__isList(c->val)); }


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
    case int_tag: *ptr=(void *) get_val(c);
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
//    case ato_tag: *s=AtomName((atom_t) c);
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
    case var_tag:  mkintg(c,i);
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
    case var_tag:  mkintg(c,i);
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
                     mkatom(c,ATOM(_true));
                   else
                     mkatom(c,ATOM(_false));
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
                     mkatom(c,ATOM(_on));
                   else
                     mkatom(c,ATOM(_off));
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
    case var_tag:  mkatom(c,A);
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
    case var_tag:  mkatom(c,ATOM(nil));
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
int unify_fun(register cell_t *c, fun_t F)
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
{ return(unify_fun(deref(t),f));
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
int unify_key(cell_t *c, cell_t *key)
{ switch(get_tag(key))
  { case ato_tag: return(PL_unify_atom(c,get_addr(key)));
    case int_tag: return(PL_unify_integer(c,get_val(key)));
    case fun_tag: return(unify_fun(c,get_fun(key)));
    default:      fail;
  }
}

#if 0
extern int do_number_vars(cell_t *c);

INLINE_DECL
int number_vars(cell_t *c)
{ typeof(TP) tp=TP;
  int n=do_number_vars(c);
  reset(tp);
  
  return(n);
}
#endif


#define	Mark(m)	do { m.trail =TP; m.global=HP; } while (0)
#define	Undo(m)	do { reset(m.trail); HP=m.global; } while(0)

int unify(cell_t *, cell_t *);

INLINE_DECL
int PL_unify(cell_t *a, cell_t *b)
{ mark_t m;
  int rval;

  Mark(m);
  if (!(rval=unify(a,b)))
    Undo(m);

  return(rval);
}


INLINE_DECL
void *AllocLocal(size_t n)
{ void *ptr=(SP+1);	// SP is postincrement.
  SP+=Adjust(n);
  return(ptr);
}

INLINE_DECL
void *AllocGlobal(size_t n)
{ void *ptr=HP;
  HP+=Adjust(n);
  return(ptr);
}

INLINE_DECL
void *AllocHeap(size_t n)
{ void *ptr=SHP;
  SHP+=Adjust(n);
  return(ptr);
}

INLINE_DECL
void *PL_foreign_context(control_t c)
{ return(((void *) c) + CELL_SIZE); }

INLINE_DECL
ctrl_t PL_foreign_control(control_t c)
{ return(*c); }

#define	GetCtxt(C)	PL_foreign_context(C)
#define	GetCtrl(C)	PL_foreign_control(C)
#define	PL_retry(C)	return(RETRY)


#define NEW(E)		((typeof(E) *) AllocHeap(sizeof(E)) )
#define NEW_(E,N)	((typeof(E) *) AllocHeap(sizeof(E)*(N)) )

#define AllocCtxt(T)	AllocLocal(sizeof(T))


#define PL_warning(fm,args...)	do { fflush(0); fprintf(stderr,"[Warning: " fm "]\n" , ## args); return(0); } while(0) 
#define PL_warn(fm,args...)	do { fflush(0); fprintf(stderr,"[Warning: " fm "]\n" , ## args); } while(0) 

#define PL_syntax_error(fm) \
	do { fflush(0); \
	     if (Sread->pos) \
	       fprintf(stderr,"[Syntax error (%ld:%ld): " fm "]\n" , \
	       		Sread->pos->line_no, Sread->pos->col_no); \
	     else \
	       fprintf(stderr,"[Syntax error : " fm "]\n"); \
	     return(0); \
	} while(0) 

/**********************************************************************/

#endif	// _FLI_H_
