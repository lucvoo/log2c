/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "Prolog.h"
#include "pl-hash.h"
#include "pl-pred.h"
#include "pl-fli.h"

typedef struct flag_t_ *flag_t;

struct flag_t_
{ cell_t key;		// Only atom, integer or first cell of a functor.
  cell_t val;		// Only atom or integer (or float).
  flag_t next;
};

#define hash_flags_size	256

static
flag_t flags[hash_flags_size];



inline static
flag_t lookup_flag(cell_t *key)
{ hash_t h;
  flag_t f;

  debut:
  switch(get_tag(key))	// get the hash value if the key is OK
  { case ref_tag: key=key->celp;
                  goto debut;
    case ato_tag:
    case fun_tag: h=key->val >> GC_BITS;
                  break;
    case int_tag: h=key->val;
		  break;
    case flt_tag:
    default:      return(0);
  }
  h=h % hash_flags_size;

  for (f=flags[h]; f!=0; f=f->next)
    if (key->val==f->key.val) return(f);	// find flag.

  f=NEW(*f);			// else create new flag
  f->key=*key;			// with this key.
  f->next=flags[h];		// insert this flag in the table
  flags[h]=f;
  f->val.val=__intg(0);		// init the value with 0
  return(f);
}
          

int pl_flag(cell_t *key, cell_t *old, cell_t *new)
{ flag_t f;
  term_t tmp;
  int n;

  f=lookup_flag(key);
  if (!f)
    PL_warning("flag/3: illegal key");

  tmp=PL_new_term_ref();
  *tmp=f->val;
  if (!pl_unify(old,tmp))
    fail;

  new=deref(new);
  if (is_atom(new))
    { f->val.celp=new; succeed; }
  else
  if (PL_eval_(new,&n))
    { f->val.val=__intg(n); succeed; }
  else
    PL_warning("flag/3: value should be an atom, integer or expression");
}


int pl_current_flag(cell_t *c, control_t ctrl)
{ flag_t flag;
  hash_t h;
  struct { hash_t hash; flag_t flag ;} *ctxt;

  switch(GetCtrl(ctrl))
  { case FIRST_CALL:
	ctxt=AllocCtxt(*ctxt);
	h=0;
	flag=flags[h];
	break;
    case NEXT_CALL:
	ctxt=GetCtxt(ctrl);
	h=ctxt->hash;
	flag=ctxt->flag;
	break;
    default:
	fail;
  }

  for (;h<hash_flags_size; flag=flags[++h])
    for (;flag; flag=flag->next)
      if (PL_unify_key(c,&(flag->key))) 	// FIXME : separe c is instantiated/variable ?
        { ctxt->hash=h;
          ctxt->flag=flag->next;
          retry;
        }

  fail;
}


/**********************************************************************/
/* flag2 : for internal use only ?                                    */
/**********************************************************************/

typedef struct flag_2_t *flag_2_t;

struct flag_2_t { cell_t key1;		// Only atoms, int, functor
		  cell_t key2;
                  cell_t val;		// Only atom or integer (or float).
                  flag_2_t next;
                };
// #define hash_flag_2_size	256
#define hash_flag_2_size	4

static
flag_2_t flag_2_tbl[hash_flag_2_size];


inline static
flag_2_t lookup_flag_2(term_t key1, term_t key2, int new)
{ hash_t h, h1, h2;
  flag_2_t f;

  key1=deref(key1);
  key2=deref(key2);

  if ( (h1=SimpleHashValue(key1)) && (h2=SimpleHashValue(key2)) )
    h=(h1+h2) % hash_flag_2_size;
  else
  { // FIXME : msg : illegal key
    return(0);
  }

  for (f=flag_2_tbl[h]; f; f=f->next)
     if ( (key1->val==f->key1.val) && (key2->val==f->key2.val) )
       return(f);		// find flag.

  if (new)
  { f=NEW(*f);			// create new flag
    f->key1= *key1;		// with these keys.
    f->key2= *key2;
    f->next=flag_2_tbl[h];	// insert this flag in the table
    flag_2_tbl[h]=f;
    return(f);
  }
  else
    return(0);			// inexistant flag
}
          
static
int PL_flag_2(term_t key1, term_t key2, term_t val)
{ flag_2_t f;

  if ((f=lookup_flag_2(key1,key2,0)))
    return(PL_unify_atomic(val,f->val));
  else
    fail;
}

int pl_flag_2(term_t key1, term_t key2, term_t val, control_t ctrl)
{ flag_2_t f;
  hash_t h;
  tr_t *tr;
  struct { hash_t hash; flag_2_t flag ;} *ctxt;

  key1=deref(key1);
  key2=deref(key2);

  if (!is_var(key1) && !is_var(key2))
    return(PL_flag_2(key1,key2,val));

  switch(GetCtrl(ctrl))
  { case FIRST_CALL:
	ctxt=AllocCtxt(*ctxt);
	h=0;
	f=flag_2_tbl[h];
	break;
    case NEXT_CALL:
	ctxt=GetCtxt(ctrl);
	h=ctxt->hash;
	f=ctxt->flag;
	break;
    default:
	fail;
  }

  tr=TP;
  for (;h<hash_flag_2_size; f=flag_2_tbl[++h])
    for (;f; f=f->next)
      if ( PL_unify_atomic(key1,f->key1) &&
           PL_unify_atomic(key2,f->key2) &&
           PL_unify_atomic(val, f->val) )
        { ctxt->hash=h;
          ctxt->flag=f->next;
          retry;
        }
      else
        reset(tr);
       

  fail;
}


static
int PL_set_flag_2(term_t key1, term_t key2, term_t val)
{ flag_2_t f;

  if ( (f=lookup_flag_2(key1,key2,1)))
  { val=deref(val);
    if (is_atom(val) || is_intg(val) || is_flt(val))
      { f->val= *val; succeed; }
    else
      PL_warning("$set_flag2/3 : instantiation fault");

    succeed;
  }
  else
    fail;
}

int pl_set_flag_2(term_t key1, term_t key2, term_t val)
{ if (PL_set_flag_2(key1,key2,val))
    succeed;
  else
    PL_warning("$set_flag2: illegal key(s)");
}

