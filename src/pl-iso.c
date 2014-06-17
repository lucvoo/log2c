/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-pred.h"
#include "pl-prims.h"
#include "pl-fli.h"
#include <stdlib.h>		// For alloca
#include "pl-atom.h"


int pl_atom_concat(term_t a1, term_t a2, term_t a3, control_t ctrl)
{ const char *s3 = 0;
  int l;
  struct { int l; const char *s; } *ctxt;

  switch(GetCtrl(ctrl))
  { case FIRST_CALL:
        { const char *s1=0, *s2=0;
          PL_get_atom_chars(a1, &s1);
	  PL_get_atom_chars(a2, &s2);
	  PL_get_atom_chars(a3, &s3);

	  if (s1 && s2)
	  { char *tmp;
            int l1, l2;

            l1 = strlen(s1);
            l2 = strlen(s2);
	    tmp = alloca(l1 + l2 + 1);
	    strcpy(tmp, s1);
	    strcpy(tmp+l1, s2);
	    return(PL_unify_atom_chars(a3, tmp));
	  }
	}

        if (!s3)
        { // PL_warning("concat/3: instantiation fault(1)");
          fail;
        }
        l=0;
        ctxt=AllocCtxt(*ctxt);
        ctxt->s=s3;
        break;
    case NEXT_CALL:
        ctxt=GetCtxt(ctrl);
        s3=ctxt->s;
        l=ctxt->l;
        break;
    default:
        fail;
  }

// Backtracking part
  { tr_t *tr;
    int l3;
    char *tmp;

    tr=TP;
    l3 = strlen(s3);
    tmp = alloca(l3 +1);
    while (l <= l3)
    { atom_t s1, s2;

      strncpy(tmp, s3, l); tmp[l]='\0';
      s1=PL_new_atom(tmp);
      s2=PL_new_atom(s3+l);
      l++;
      if ( PL_unify_atom(a1, s1) &&
           PL_unify_atom(a2, s2) )
      { ctxt->l=l;
        retry;
      }
      else
      { reset(tr);
      }
    }
    fail;
  }
}


int pl_sub_atom(term_t A, term_t L1, term_t L2, term_t L3, term_t A2,
		control_t ctrl)
{ const char *a = 0, *a2 = 0;
  int t, l1, l2, l3;
  struct { int t; int l1; int l2; int l3; const char *a; } *ctxt;

  switch(GetCtrl(ctrl))
  { case FIRST_CALL:
        PL_get_atom_chars(A, &a);

	if (!a)
        { // PL_warning("concat/3: instantiation fault(1)");
          fail;
        }

        l1=0;
        l2=0;
        l3=0;
        t=0;

        if (PL_get_intg(L1, &l1))
        { t=1;
        }
        else
        if (PL_get_intg(L2, &l2))
        { t=2;
        }
        else
        if (PL_get_intg(L3, &l3))
        { t=3;
        }
        else
        if (PL_get_atom_chars(A2, &a2))
        { t=4;
        }

        ctxt=AllocCtxt(*ctxt);
        ctxt->t=t;
        ctxt->a=a;
        ctxt->l1=l1;
        ctxt->l2=l2;
        ctxt->l3=l3;
        break;
    case NEXT_CALL:
        ctxt=GetCtxt(ctrl);
        t =ctxt->t;
        a =ctxt->a;
        l1=ctxt->l1;
        l2=ctxt->l2;
        l3=ctxt->l3;
        break;
    default:
        fail;
  }

// Backtracking part
  { tr_t *tr;
    int l;
    char *tmp;

    tr=TP;
    l = strlen(a);
    tmp = alloca(l+1);

    switch (t)
    { case 0:
	while (l1 <= l)
	{ while (l2 <= l -l1)
	  { atom_t s2;
	    int l3 = l -l1 -l2;

	    strncpy(tmp, a+l1, l2); tmp[l2]='\0';
	    s2=PL_new_atom(tmp);
	    if ( PL_unify_atom(A2, s2) &&
	         PL_unify_intg(L1, l1) &&
	         PL_unify_intg(L2, l2) &&
	         PL_unify_intg(L3, l3)
	       )
	    { l2++;
	      ctxt->l1=l1;
	      ctxt->l2=l2;
	      retry;
	    }
	    else
	    { l2++;
	      reset(tr);
	    }
	  }
	  l2=0;
	  l1++;
	}
        break;

      case 1:
	while (l2 <= l -l1)
	{ atom_t s2;
	  int l3 = l -l1 -l2;

	  strncpy(tmp, a+l1, l2); tmp[l2]='\0';
	  s2=PL_new_atom(tmp);
	  if ( PL_unify_atom(A2, s2) &&
	       // PL_unify_intg(L1, l1) &&
	       PL_unify_intg(L2, l2) &&
	       PL_unify_intg(L3, l3)
	     )
	  { l2++;
	    ctxt->l2=l2;
	    retry;
	  }
	  else
	  { l2++;
	    reset(tr);
	  }
	}
        break;

      case 2:
	while (l1 <= l -l2)
	{ atom_t s2;
	  int l3 = l -l1 -l2;

	  strncpy(tmp, a+l1, l2); tmp[l2]='\0';
	  s2=PL_new_atom(tmp);
	  if ( PL_unify_atom(A2, s2) &&
	       PL_unify_intg(L1, l1) &&
	       // PL_unify_intg(L2, l2) &&
	       PL_unify_intg(L3, l3)
	     )
	  { l1++;
	    ctxt->l1=l1;
	    retry;
	  }
	  else
	  { l1++;
	    reset(tr);
	  }
	}
        break;

      case 3:
	while (l1 <= l -l3)
	{ atom_t s2;
	  int l2 = l -l1 -l3;

	  strncpy(tmp, a+l1, l2); tmp[l2]='\0';
	  s2=PL_new_atom(tmp);
	  if ( PL_unify_atom(A2, s2) &&
	       PL_unify_intg(L1, l1) &&
	       PL_unify_intg(L2, l2) &&
	       1
	       // PL_unify_intg(L3, l3)
	     )
	  { l1++;
	    ctxt->l1=l1;
	    retry;
	  }
	  else
	  { l1++;
	    reset(tr);
	  }
	}
        break;

      case 4:
        PL_get_atom_chars(A2, &a2);
	while ((tmp = strstr(a+l1,a2)))
	{ int l2 = strlen(a2);
          l1=tmp-a;
          l3 = l - l1 - l2;

	  if ( // PL_unify_atom(A2, s2) &&
	       PL_unify_intg(L1, l1) &&
	       PL_unify_intg(L2, l2) &&
	       PL_unify_intg(L3, l3)
	     )
	  { l1++;
	    ctxt->l1=l1;
	    retry;
	  }
	  else
	  { l1++;
	    reset(tr);
	  }
	}
        break;
      }

    fail;
  }
}

/********************************************************/

