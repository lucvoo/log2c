/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "Prolog.h"
#include "pl-fli.h"
#include "pl-atom.h"
#include "pl-string.h"


static atom_t atoms_tab[256];

atom_t PL_char_to_atom(int c)
{ static char tmp[2];

  if (0>c || c>255)
    return(0);

  if (!atoms_tab[c])
  { tmp[0]=c; tmp[1]='\0';
    atoms_tab[c]=PL_new_atom(tmp);
  }
  return(atoms_tab[c]);
}


term_t PL_mk_code_list(char *s)
{ term_t t=HP;

  for (;*s;s++)
  { HP[0].val=__cons();
    HP[1].val=__intg((unsigned)*s);
    HP+=2;
  }
  HP[0].val=__nil();
  HP++;
  return(t);
}


term_t PL_mk_char_list(char *s)
{ term_t t=HP;

  for (;*s;s++)
  { HP[0].val=__cons();
    HP[1].val=__atom(PL_char_to_atom(*s));
    HP+=2;
  }
  HP[0].val=__nil();
  HP++;
  return(t);
}


int pl_char_code(term_t Ch, term_t Co)
{ int c;
  atom_t a;
  const char *s;

  if (PL_get_intg(Co, &c))
  { if ((a=PL_char_to_atom(c)))
      return(PL_unify_atom(Ch,a));
    else
    { // Representation_error ...
      fail;
    }
  }

  if (PL_get_atom_chars(Ch,&s))
  { if (s[0] != 0 && s[1] == 0)		// one char atom
      return(PL_unify_intg(Co,s[0]));
    else
    { // Type_error ...
      fail;
    }
  }
  
  fail;
}
