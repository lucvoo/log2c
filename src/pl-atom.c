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
  a->atom.val=((ato_tag<<29)+(uint) a);
  a->name=s;
  a->hash=H;
  a->next=atoms[h];
  atoms[h]=a;

  return(a);
}

atom_t lookup_atom(const char *s)
{ hash_t h, H;
  atom_t a;
  const char *copy;

  H=hpjw(s);
  h=H % hash_atoms_size;

  for (a=atoms[h];a!=0;a=a->next)
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
	if (isAtom(c))	succeed;
	if (!isVar(c)) fail;	

	ctxt=AllocCtxt(*ctxt);
	h=0;
	atom=atoms[h];
	break;
    case NEXT_CALL:
	ctxt=GetCtxt(ctrl);
	h=ctxt->hash;
	atom=ctxt->atom;
	break;
    default:
	fail;
  }

  for (;h<hash_atoms_size; atom=atoms[++h])
    if (atom)
      { PL_unify_atom(c,atom);	// Always succeed since c is a var !
        ctxt->hash=h;
        ctxt->atom=atom->next;
	retry;
      }

  fail;
}


