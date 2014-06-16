/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include <stdlib.h>	// for qsort()

int pl_is_list(term_t l)
{ return(PL_is_cons(l)); }

int pl_proper_list(term_t l)
{ Deref(l);

  while(is_cons(l))
    l=deref(l+2);
   
  return(is_nil(l));
}

int pl_partial_list(term_t l)
{ Deref(l);

  while(is_cons(l))
    l=deref(l+2);
   
  return(is_var(l));
}

int pl_memberchk(term_t e, term_t l)
{ Deref(l);
  Deref(e);

  while(is_cons(l))
  { if (PL_try_unify(e,l+1))
      succeed;
    l=deref(l+2);
  }
  fail;
}

int pl_length(term_t list, term_t l)
{ int m;
  Deref(list);

  if (PL_get_intg(l,&m))
  { register int n=m;

    while (is_cons(list) && n>0)
    { n--; list=deref(list+2); }
     
    if (n==0 && is_nil(list)) succeed;
    else
    if (!is_var(list) || n<0) fail;
    else	// var(list) && n>= 0
    { term_t c;
      list->celp=HP;
      trail(list);
      c=HP;
      HP+=(2*n);
      while (c<HP)
      { c[0].val=__cons();
        c[1].val=__var();
        c+=2;
      }	// POST : c==HP
      HP[0].val=__nil();
      HP++;
      succeed;
    }
  }
  else
  if (PL_is_var(l))
  { int n=PL_lengthList(list);
    if (n>=0) return(PL_unify_intg(l,n));
  }

  fail;
}


/**********************************************************************/
/* Stuff for sort/2 and msort/2                                       */
/**********************************************************************/

// Return length of the list; -1 if not a proper_list
// Put the array in static_heap
inline static
int list_to_array(term_t list)
{ term_t l;
  int n=0;
  
  l=deref(list);
  while(is_cons(l))  
  { (SHP++)->celp=l+1;
    l=deref(l+2);
    n++;
  }

  if (is_nil(l))
    return(n);
  else
    return(-1);
}

inline static
term_t array_to_list(term_t *array, int n, int rem_dup)
{ term_t l=HP;
  term_t last;
  
  while(n--)  
  { HP[0].val=__cons();
    HP[1].celp=last=*array++;
    HP+=2;

    if (rem_dup)
      while (n && pl_std_cmp(last,*array)==0)
      { n--; array++; }
  }

  HP[0].val=__nil();
  HP++;
  return(l);
}


inline static
int PL_sort(term_t list, term_t sorted, int rem_dup)
{ term_t *array=(term_t *) SHP;
  term_t l;
  int n;

  n=list_to_array(list);
  SHP=(void *)array;

  if (n<0)
    PL_warning("%s/2: first_argument is not a proper list",rem_dup ? "sort" : "msort");

  if (n!=0)
    qsort(array,n,sizeof(term_t),pl_std_cmp);
   
  l=array_to_list(array,n,rem_dup);
  return(pl_unify(l,sorted));
}

int pl_sort(term_t list, term_t sorted)
{ return(PL_sort(list,sorted,1)); }

int pl_msort(term_t list, term_t sorted)
{ return(PL_sort(list,sorted,0)); }


