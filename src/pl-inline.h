/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_INLINE_H_
#define PL_INLINE_H_

#include "pl.h"


#define ATOM(A)		(&ATOM_ ## A ## )
#define FUN_(F)		(&FUN_ ## F ## )
#define FUN(F,A)	(&FUN_ ## F ## _ ## A ## )

#define Round(i,align)	((i+align-1) & (-align) )

#define Align_N(ptr,align)	((void *) Round((int) ptr,align) )
#define Align(ptr)		Align_N(ptr,CELL_SIZE)

#define Adjust_N(i,align)	((i+align-1) / align )
#define Adjust(i)		Adjust_N(i,CELL_SIZE)


#define get_val(c)	((c)->tag_val.val)
#define get_uval(c)	((c)->tag_uval.uval)
#define get_addr(c)	((void *) ((c)->tag_uval.uval))
#define get_tag(c)	((c)->tag_val.tag)

#define get_fun(c)	((fun_t) get_addr(c))
#define get_atom(c)	((atom_t) (c))
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

#define MK_CELL(T,V)	((T<<TAG_POS)+(pl_word_t) (V))
#define new_atom(A)	(&((A)->atom))
#define __cons()	__fun(FUN(dot,2))
#define __nil()		__atom(ATOM(nil))

INLINE_DECL
pl_word_t __intg(intg_t N)
{ return(MK_CELL(int_tag, VAL_MASK & N)); }

INLINE_DECL
pl_word_t __fun(fun_t F)
{ return(MK_CELL(fun_tag,F)); }

INLINE_DECL
pl_word_t __var(void)
{ return(MK_CELL(var_tag,0)); }

INLINE_DECL
pl_word_t __atom(atom_t A)
{ return((pl_word_t) new_atom(A)); }


INLINE_DECL
int is_ref(cell_t *c)
{ return(get_tag(c)==ref_tag); }

INLINE_DECL
int is_var(cell_t *c)
{ return(c->val==__var()); }

INLINE_DECL
int is_atom(cell_t *c)
{ return(get_tag(c)==ato_tag); }

INLINE_DECL
int is_intg(cell_t *c)
{ return(get_tag(c)==int_tag); }

INLINE_DECL
int is_flt(cell_t *c)
{ return(get_tag(c)==flt_tag); }

INLINE_DECL
int is_number(cell_t *c)
{ int tag=get_tag(c);
  return(tag==int_tag || tag==flt_tag);
}

INLINE_DECL
int is_atomic(cell_t *c)
{ int tag=get_tag(c);
  return(tag==ato_tag || tag==int_tag || tag==flt_tag);
}

INLINE_DECL
int is_fun(cell_t *c)
{ return(get_tag(c)==fun_tag); }

INLINE_DECL
int is_term(cell_t *c)
{ return(is_fun(c)); }


INLINE_DECL
cell_t *new_intg(long N)
{ HP->val = __intg(N);
  return(HP++);
}

INLINE_DECL
cell_t *new_flt(double r)
{ typeof(HP) old_HP=HP;

  HP->val = (flt_tag<<29);
  get_flt(HP)=r;
  HP+=3;
  return(old_HP);
}

INLINE_DECL 
cell_t *new_var(void)
{ HP->val = __var();
  return(HP++);
}

INLINE_DECL
cell_t *new_void(void)
{ return(new_var()); }		// FIXME : put void var in local stack

INLINE_DECL 
cell_t *new_struct(fun_t F, int N)
{ register typeof(HP) old_HP;

  old_HP=HP;
  HP->val = __fun(F);
  HP+=(N+1);

  return(old_HP);
}

INLINE_DECL
cell_t *new_cons(void)
{ return(new_struct(FUN(dot,2),2)); }

INLINE_DECL 
int isatom(atom_t A, cell_t *addr)
{ return( addr == &(A->atom) ); // atoms are unique !
}

INLINE_DECL 
int isintg(long N, cell_t *addr)
{ return( addr->val == __intg(N)); }

INLINE_DECL 
int isflt(double r, cell_t *addr)
{ return( get_flt(addr) == r); }


INLINE_DECL 
int isfun(fun_t F, cell_t *addr)
{ return( addr->val == __fun(F) ); }


cell_t *deref_dbg(cell_t *addr);

INLINE_DECL 
cell_t *deref(cell_t *addr)
#if 1
{ cell_t *p=addr;

  while (p->tag_val.tag==ref_tag)
  { p=p->celp;
  }

  return(p);
}
#else
{ return deref_dbg(addr); }
#endif

#define Deref(addr)	while (get_tag(addr)==ref_tag) addr=addr->celp


INLINE_DECL 
void mkref(cell_t *v, cell_t c)
{ *v=c;

  return;
}

INLINE_DECL 
void mkrefp(cell_t *v, cell_t *c)
{ v->celp=c;

  return;
}


INLINE_DECL 
void trail(cell_t *addr)
{ if (addr < (BTP+3)->celp)
     *(TP++)=addr;

  return;
}

INLINE_DECL
void reset(register tr_t *a1)
{ register tr_t *tp;

  for (tp=TP;a1<tp;)
     (*(--tp))->val = var_tag<<29;

  TP=a1;
  return;
}

INLINE_DECL
void backtrack(void)
{ FP=BTP;
  HP=(FP+3)->celp;
  reset(FP[2].tr);
  goto *((FP+4)->cod);
}


INLINE_DECL
unsigned long round_to_power(unsigned long n)
{ int r=1;
  n=n-1;

  do { n/=2; r=r*2; }
  while(n!=0);

  return(r);
}


// 0 and -1 are never a valid value
INLINE_DECL
hash_t SimpleHashValue(cell_t *key)
{ debut:
  switch(get_tag(key))	// get the hash value if the key is OK
  { case ref_tag: key=key->celp;
                  goto debut;
    case ato_tag:
    case fun_tag: return((key->val) >> GC_BITS);
    case int_tag: return(key->val);
    default:      return(0);
  }
}


#endif	// PL_INLINE_H_
