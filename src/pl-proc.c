/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "Prolog.h"
#include "pl-fli.h"
#include "pl-write.h"

extern cell_t *ARGS[17] asm("ARG_0");
cell_t *ARG_0 = 0;
cell_t *ARG_1 = 0;
cell_t *ARG_2 = 0;
cell_t *ARG_3 = 0;
cell_t *ARG_4 = 0;
cell_t *ARG_5 = 0;
cell_t *ARG_6 = 0;
cell_t *ARG_7 = 0;
cell_t *ARG_8 = 0;
cell_t *ARG_9 = 0;
cell_t *ARG_10 = 0;
cell_t *ARG_11 = 0;
cell_t *ARG_12 = 0;
cell_t *ARG_13 = 0;
cell_t *ARG_14 = 0;
cell_t *ARG_15 = 0;
cell_t *ARG_16 = 0;


extern modules_t *PL__modules[];
extern module_t module_system;
extern int PL__modules_hash_size;

inline static
module_t *lookup_module(atom_t name)
{ hash_t h;
  modules_t *m;

  h=name->hash % PL__modules_hash_size;
  for (m=PL__modules[h];m!=0;m=m->next)
    if (m->name==name)
      return(m->module);

  return(0);
}

inline static
void *lookup_proc(module_t *module, atom_t functor, int arity)
{ hash_t h;
  jmp__t *j;

  h=(functor->hash+arity) % module->all.size;
  for (j=module->all.tab[h];j!=0;j=j->next)
  { if (j->functor==functor && j->arity==arity)
      return(j->pred);
  }

// if fail : try default module : system	// FIXME
  module=&module_system;
  h=(functor->hash+arity) % module->all.size;
  for (j=module->all.tab[h];j!=0;j=j->next)
  { if (j->functor==functor && j->arity==arity)
      return(j->pred);
  }

  PL_write(Stderr, (term_t) functor);
  PL_warning("/%d lookup_proc : no such procedure",arity);
}


inline static
term_t strip_module(term_t term, module_t **module)
{ term_t m=0, t;
  module_t *mod;

  t=deref(term);
  while(t->val==__fun(FUN(module,2)))
  { m=t+1;
    t=deref(t+2);
  }

  if (!m) // if no module given : try default module : system // FIXME
    m=new_atom(ATOM(_system));
  else
    m=deref(m);

  if (is_atom(m) && (mod=lookup_module(get_atom(m))))
  { *module=mod;
    return(deref(t));
  }

  PL_write(Stderr, term);
  PL_warning(" : strip_module : illegal module:term specification");
}


void *PL_call(term_t clos, int extra, term_t *args)
{ term_t t;
  atom_t name;
  int arity, n;
  module_t *mod;
  void *proc;

  if (!(t=strip_module(clos, &mod)) ||
      !PL_get_name_arity(t,&name,&arity) ||
      !(proc=lookup_proc(mod,name,arity+extra)) )
  { PL_write(Stderr, clos);
    PL_warning("PL_call : fail");
  }
   
  for (n=1;n<=arity;n++) ARGS[n]=deref(t+n);
  for (n=0;n<extra;n++)  ARGS[arity+1+n]=args[n];

  return(proc);
}

void *PL_apply(term_t clos, term_t list)
{ term_t t;
  atom_t name;
  int arity, n, extra;
  module_t *mod;
  void *proc;

  list=deref(list);

  if (!(t=strip_module(clos, &mod))		||
      !PL_get_name_arity(t,&name,&arity)	||
      !((extra = PL_lengthList(list))>=0)	||
      !(proc = lookup_proc(mod,name,arity+extra)) )
  { PL_write(Stderr, clos);
    PL_warning("PL_apply : fail");
  }
   
  for (n=1;n<=arity;n++)
    ARGS[n]=deref(t+n);
  for (n=0;n<extra;n++)
  { ARGS[arity+1+n]=deref(list+1);
    list=deref(list+2);
  }

  return(proc);
}
