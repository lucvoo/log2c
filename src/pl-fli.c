/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "Prolog.h"
#include "pl-atom.h"
#include "pl-fun.h"
#include "pl-string.h"
#include "pl-pred.h"
#include "pl-fli.h"
#include "pl-buffer.h"


cell_t *
PL_new_term_refs(int n)
{ cell_t *r=HP;
  
  for (;n>0;n--)
    HP[n].val=__var();

  HP+=n;
  
  return r;
}

cell_t *
PL_new_term_ref(void)
{ cell_t *r=HP;
  
  HP->val=__var();
  HP++;
  
  return r;
}



		 /*******************************
		 *	       ATOMS		*
		 *******************************/

atom_t
PL_new_atom(const char *s)
{ return PL_lookup_atom(s); }

const char *
PL_atom_chars(atom_t a)
{ return a->name; }

functor_t
PL_new_functor(atom_t f,  int a)
{ return PL_lookup_fun(f, a); }

atom_t
PL_functor_name(functor_t f)
{ return f->functor; }

int
PL_functor_arity(functor_t f)
{ return f->arity; }


		 /*******************************
		 *	       CONS-*		*
		 *******************************/
#include <stdarg.h>
void
PL_cons_functor(term_t h, functor_t fd, ...)
{ int arity = fd->arity;
  cell_t *f;
  va_list args;

  va_start(args, fd);
  h->celp=f=HP;
  HP+=(arity+1);
  f->val=__fun(fd);

  for (f++;f<HP;f++)
    f->celp=deref(va_arg(args, term_t));

  va_end(args);

}


void
PL_cons_list(term_t l, term_t head, term_t tail)
{ cell_t *a;
  
  l->celp=a=HP;
  HP+=3;

  (a)->val=__cons();
  (a+1)->celp=deref(head);
  (a+2)->celp=deref(tail);
}


		 /*******************************
		 *	      GET-*		*
		 *******************************/

static
char *malloc_string(char *s)
{ int n;
  char *m;

  n=strlen(s)+1;
  m=malloc(n);		// FIXME : check for malloc
  memcpy(m,s,n);
 
  return(m);
}


int PL_get_list_codes(term_t list, const char **s, unsigned flags)
{ pl_ubs_t *b = PL_find_ubs(flags);
  term_t l;
  static int c;

  l=deref(list);
  while (is_cons(l))
  { if (!PL_get_intg(l+1,&c) || c!=(c&0xff))
      goto failed;

    PL_add_ubs(b,c);
    l=deref(l+2);
  }
  if (!is_nil(l)) goto failed;

  PL_add_ubs(b,'\0');
  *s=PL_base_ubs(b);
  succeed;

failed:
  PL_lost_ubs(flags);
  fail;
}

int PL_get_list_chars(term_t list, const char **s, unsigned flags)
{ pl_ubs_t *b = PL_find_ubs(flags);
  term_t l;

  l=deref(list);
  while (is_cons(l))
  { const char *a;

    if (!PL_get_atom_chars(l+1,&a))
      goto failed;

    if (a[0] == '\0' || a[1] != '\0')
      goto failed;

    PL_add_ubs(b,a[0]);
    l=deref(l+2);
  }
  if (!is_nil(l)) goto failed;

  PL_add_ubs(b,'\0');
  *s = PL_base_ubs(b);
  succeed;

failed:
  PL_lost_ubs(flags);
  fail;
}


int
PL_get_chars(term_t term, const char **s, unsigned flags)
{ term_t t;
  static char tmp[24];		// FIXME : why 24 ??
  char *r;
  int type;

  t=deref(term);

  if ((flags & CVT_ATOM) && is_atom(t))
  { type = ato_tag;
    r = (char *) get_a_name(t);
  }
  else
  if ((flags & CVT_INTEGER) && is_intg(t))
  { type = int_tag;
    sprintf(tmp, "%ld", (long) get_val(t));
    r = tmp;
  }
  else
  if (flags & CVT_LIST)
    return(PL_get_list_codes(t,s,flags));
  else
  if ( (flags & CVT_FLOAT) && is_flt(t) )
  { type = flt_tag;
    sprintf(tmp, "%f", get_flt(t) );
    r = tmp;
  }
  else
#if 0	// FIXME : add varName(V). -> static in pl-write.c
  if ((flags & CVT_VARIABLE) && is_var(t))
  { type = var_tag;
    r = varName(t);
  }
  else
#endif
    fail;
    
  if ( flags & BUF_MALLOC )
  { *s = malloc_string(r);
  }
  else
  if ( (flags & BUF_RING && type != ato_tag) )
  { pl_ubs_t *b = PL_find_ubs(flags);
    int l = strlen(r) + 1;

    PL_add_x_ubs(b,r,l);
    *s = PL_base_ubs(b);
  }
  else
    *s = r;

  succeed;
}


int
PL_get_name_arity(term_t t, atom_t *name, int *arity)
{ Deref(t);

  if (is_fun(t))
  { functor_t f = get_fun(t);

    *name =  f->functor;
    *arity = f->arity;
    succeed;
  }
  else
  if (is_atom(t))
  { *name = get_atom(t);
    *arity = 0;
    succeed;
  }

  fail;
}


int
PL_get_functor(term_t t, functor_t *f)
{ Deref(t);

  if ( isTerm(t) )
  { *f = get_fun(t);
    succeed;
  }
  if ( isAtom(t) )
  { *f = PL_lookup_fun(get_atom(t), 0);
    succeed;
  }

  fail;
}



// int
// PL_get_arg_(int index, term_t t, term_t *a)
// { Deref(t);
// 
//   if (is_fun(t))
//   { int arity = get_arity(t);
// 
//     if ( index >0 && index <= arity )
//     { *a=t+index;
//       succeed;
//     }
//   }
// 
//   fail;
// }

int
PL_get_arg(int index, term_t t, term_t a)
{ Deref(t);

  if (isTerm(t))
  { int arity = get_arity(t);

    if ( index >0 && index <= arity )
    { mkrefp(a,deref(t+index));
      succeed;
    }
  }

  fail;
}


int
PL_get_list(term_t l, term_t h, term_t t)
{ Deref(l);

  if (isCons(l))
  { mkrefp(h,(l+1));
    mkrefp(t,(l+2));
    succeed;
  }
  fail;
}

int 
PL_get_list_(term_t l, term_t *h, term_t *t)
{ Deref(l);

  if (is_cons(l))
  { *h=l+1;
    *t=l+2;
    succeed;
  }
  fail;
}


int
PL_get_head(term_t l, term_t h)
{ Deref(l);

  if (isList(l))
  { mkrefp(h,(l+1));
    succeed;
  }
  fail;
}

int
PL_get_tail(term_t l, term_t t)
{ Deref(l);

  if (isList(l))
  { mkrefp(t,(l+2));
    succeed;
  }
  fail;
}


int
PL_get_nil(term_t l)
{ return(isNil(l));
}



		 /*******************************
		 *		IS-*		*
		 *******************************/

int
PL_is_variable(term_t t)
{ return isVar(t);
}


int
PL_is_atom(term_t t)
{ return isAtom(t);
}


int
PL_is_integer(term_t t)
{ return isInteger(t);
}


int
PL_is_compound(term_t t)
{ return isTerm(t);
}


int
PL_is_functor(term_t t, functor_t f)
{ return(f==get_fun(deref(t)));
}


int
PL_is_list(term_t t)
{ return(isList(t));
}


int
PL_is_atomic(term_t t)
{ return(isAtomic(t));
}


int
PL_is_number(term_t t)
{ return(isNumber(t));
}



		 /*******************************
		 *             PUT-*  		*
		 *******************************/

void
PL_put_variable(term_t t)
{ mkrefp(t,new_var());
}


void
PL_put_atom(term_t t, atom_t a)
{ mkatom(t,a);
}


void
PL_put_atom_chars(term_t t, const char *s)
{ atom_t a=PL_lookup_atom(new_str(s));
  PL_put_atom(t,a);
}


void
PL_put_integer(term_t t, long i)
{ mkintg(t,i);
}



void
PL_put_functor(term_t t, functor_t f)
{ int arity = f->arity;

  if ( arity == 0 )
  { mkatom(t,f->functor);
  } else
  { cell_t *a=new_struct(f,arity);

    while(arity-- > 0)
      (++a)->val=__var();

    mkrefp(t,a);
  }
}


void
PL_put_list(term_t l)
{ cell_t *a = new_cons();

  a[1].val=a[2].val=__var();
  mkrefp(l,a);
}


void
PL_put_nil(term_t l)
{ mkatom(l,ATOM(nil));
}


void
PL_put_term(term_t t1, term_t t2)
{ mkrefp(t1,deref(t2));
}


		 /*******************************
		 *	       UNIFY		*
		 *******************************/

int
PL_unify_atom_chars(term_t t, const char *chars)
{ return(PL_unify_atom(t,PL_lookup_atom(new_str(chars))));
}



int
PL_unify_arg(int index, term_t t, term_t a)
{ Deref(t);

  if ( (get_tag(t)==fun_tag) && index > 0 && index <= get_arity(t) )
    return pl_unify(a,t+index);

  fail;
}


		 /*******************************
		 *	       TYPE		*
		 *******************************/


int
PL_term_type(term_t t)
{ return(Tag(t->val)>>29);
}

