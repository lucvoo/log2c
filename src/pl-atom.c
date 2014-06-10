/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "Prolog.h"
#include "pl-hash.h"
#include "pl-string.h"
#include "pl-atom.h"


inline static
atom_t add_atom(const char *s, hash_t H, hash_t h)
{ atom_t a;

  a=NEW(*a); 
  a->atom.val=((ato_tag<<TAG_POS)+(unsigned int) a);
  a->name=s;
  a->hash=H;
  a->next=PL__atoms[h];
  PL__atoms[h]=a;
  PL__atoms_count++;

  return(a);
}

atom_t PL_new_atom(const char *s)
{ hash_t h, H;
  atom_t a;
  const char *copy;

  H=PL_hpjw(s);
  h=H % PL__atoms_hash_size;

  for (a=PL__atoms[h];a!=0;a=a->next)
    { if (streq(s,a->name))
        return(a);
    }

  copy=new_str(s);
  return(add_atom(copy,H,h));
}



int pl_current_atom(cell_t *c, control_t ctrl)
{ atom_t atom;
  hash_t h;
  struct { hash_t hash; atom_t atom ;} *ctxt;

  switch(GetCtrl(ctrl))
  { case FIRST_CALL:
	if (PL_is_atom(c))	succeed;
	if (!PL_is_var(c)) fail;	

	ctxt=AllocCtxt(*ctxt);
	h=0;
	atom=PL__atoms[h];
	break;
    case NEXT_CALL:
	ctxt=GetCtxt(ctrl);
	h=ctxt->hash;
	atom=ctxt->atom;
	break;
    default:
	fail;
  }

  for (;h<PL__atoms_hash_size; atom=PL__atoms[++h])
    if (atom)
      { PL_unify_atom(c,atom);	// Always succeed since c is a var !
        ctxt->hash=h;
        ctxt->atom=atom->next;
	retry;
      }

  fail;
}


