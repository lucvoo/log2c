/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "Prolog.h"
#include "pl-hash.h"
#include "pl-string.h"
#include "pl-fun.h"

inline static
fun_t add_fun(atom_t functor, int arity, hash_t h)
{ fun_t f;

  f=NEW(*f); 
  f->functor=functor;
  f->arity=arity;
  f->next=PL_funs[h];
  PL_funs[h]=f;
  PL_funs_count++;

  return(f);
}


inline static
int exist_fun(atom_t functor, int arity)
{ hash_t h;
  fun_t f;

  h=(functor->hash + arity) % PL_funs_hash_size;

  for (f=PL_funs[h];f!=0;f=f->next)
    { if (f->functor==functor && f->arity==arity)
        succeed;
    }

  fail;
}

fun_t lookup_fun(atom_t functor, int arity)
{ hash_t h;
  fun_t f;
				// FIXME : test if arity == 0 ???
  h=(functor->hash + arity) % PL_funs_hash_size;

  for (f=PL_funs[h];f!=0;f=f->next)
  { if (f->functor==functor && f->arity==arity)
      return(f);
  }

  return(add_fun(functor,arity,h));
}



int pl_current_functor(cell_t *f, cell_t *n, control_t ctrl)
{ fun_t fun;
  hash_t h;
  typedef enum { functor, arity, all } type_t;
  struct { hash_t hash; fun_t fun; type_t type; } *ctxt;

  Deref(f); Deref(n);

  switch(GetCtrl(ctrl))
  { case FIRST_CALL:
	if (is_atom(f))
        { if (is_intg(n))
            return(exist_fun(get_atom(f),get_val(n)));
          else
          if (!is_var(n))
            fail;
          else
          { ctxt=AllocCtxt(*ctxt);
            ctxt->type=arity;
            h=0;
            fun=PL_funs[0];
            goto loop_arity;
          }
        }
        else
        if (!is_var(f))
          fail;
        else
        if (is_intg(n))
        { ctxt=AllocCtxt(*ctxt);
          ctxt->type=functor;
          h=0;
          fun=PL_funs[0];
          goto loop_functor;
        }
        else
        if (!is_var(n)) fail;
        else
        { ctxt=AllocCtxt(*ctxt);
          ctxt->type=all;
          h=0;
          fun=PL_funs[0];
          goto loop_all;
        }

    case NEXT_CALL:
	ctxt=GetCtxt(ctrl);
	h=ctxt->hash;
	fun=ctxt->fun;
        { int t=ctxt->type;
          if (t==functor) goto loop_functor;
          if (t==arity)   goto loop_arity;
          if (t==all)     goto loop_all;
        }
    default:
	fail;
  }

  loop_arity:	// INV: f is instantiated;
                //      n is a variable.
  for (;h<PL_funs_hash_size; fun=PL_funs[++h])
    for (;fun; fun=fun->next)
      if ( fun->functor==get_atom(f))
        { mkintg(n,fun->arity); trail(n); 
          ctxt->hash=h;
          ctxt->fun=fun->next;
          retry;
        }  
  fail;

  loop_functor:	// INV: f is a variable
                //      n is instantiated.
  for (;h<PL_funs_hash_size; fun=PL_funs[++h])
    for (;fun; fun=fun->next)
      if ( fun->arity==get_val(n))
        { mkatom(f,fun->functor); trail(f); 
          ctxt->hash=h;
          ctxt->fun=fun->next;
          retry;
        }  
  fail;

  loop_all:	// INV: f is a variable
                //      n is a variable.
  for (;h<PL_funs_hash_size; fun=PL_funs[++h])
    for (;fun; fun=fun->next)
      { mkatom(f,fun->functor); trail(f); 
        mkintg(n,fun->arity); trail(n); 
        ctxt->hash=h;
        ctxt->fun=fun->next;
        retry;
      }  
  fail;
}


