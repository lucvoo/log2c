/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "Prolog.h"
#include "pl-hash.h"
#include "pl-pred.h"
#include "pl-fli.h"

static cell_t *base;

inline static
void rtrail(cell_t *ref)
{ HP->celp = ref;
  HP++;
}


typedef struct record__t *rec_t;
typedef struct reclist_t *recl_t;
struct record__t { recl_t list;
                   rec_t  next;
		   int    size;
                   cell_t term[0];
                 };
struct reclist_t { cell_t key;
                   rec_t  first;
                   rec_t  last;
                   recl_t next;
                 };

#define hash_recs_size	256
static
recl_t records[hash_recs_size];


static
cell_t *Copy2Heap(cell_t *addr, cell_t *c)
{ debut:
  switch(get_tag(c))
  { case ref_tag: c=c->celp;
                  goto debut;
    case var_tag: if ((c>=base) && (c<SHP)) // ref to the copy
		  { addr->celp=c;
		    return(c);
                  }
                  else                     // new var
                  if (addr)		    // write to a fun object.
                  { addr->val=(var_tag<<29);
                    c->celp=addr;
                    trail(c);
                    return(0);
                  }
                  else			    // single object
                  { addr=new_var();
                    c->celp=addr;
                    trail(c);
                    return(addr);
                  }
    case ato_tag: if (!addr)
                    addr=NEW(cell_t);
                  addr->celp=c;
                  return(addr);
    case int_tag: if (!addr)
                    addr=NEW(cell_t);
                  addr->val=c->val;
                  return(addr);
    case fun_tag: { int n=get_arity(c);
                    if (!addr)
                      addr=NEW_(cell_t,n+1);  
                    else
                    { addr->celp=NEW_(cell_t,n+1);
		      addr=addr->celp;
                    }

                    addr->val=c->val;
                    for (;n>0;n--)
                      Copy2Heap(addr+n,c+n);

                    return(addr);
                  }
  }
  return(0);
}

inline static
rec_t copy_to_heap(cell_t *c)
{ rec_t   record; 
  tr_t *tr;

  tr=TP;
  record=NEW(*record);
  base=SHP;			// == record->term == record+1;
  Copy2Heap(0, c);

  record->size=SHP-base;	// == SHP-record->term
  reset(tr);
  return(record);
}


inline static
cell_t *copy_to_global(rec_t record)
{ int n, i;
  cell_t *c;

  c=record->term;
  n=record->size;
  for (i=0;i<n;i++)
  { if (is_ref(c+i ) && c[i].celp >= c && c[i].celp < c+n )
      HP[i].celp=(c[i].celp)+(HP-c); 
    else
      HP[i].val=c[i].val;
  }

  c=HP;
  HP+=record->size;

  return(c);
}
 
/**********************************************************************/
/* unify_static == unify in the record without first copy the term    */
/**********************************************************************/

#define Trail(T)	*TP++=T

static inline
int unify_static_2(register cell_t *s, register cell_t *t)
{ s=deref(s);

  debut:
  if (s==t) goto OK;			// same object

  switch(get_tag(t))
    { case ref_tag: t=t->celp;
		    goto debut;
      case var_tag: mkrefp(t,s);
                    Trail(t);
                    goto OK;

      case ato_tag: if (is_var(s))
                    { mkrefp(s,t);
                      Trail(s);
                      goto OK;
                    }
                    else goto KO;

      case int_tag: if (s->val==t->val) goto OK;
                    if (is_var(s))
                    { s->val=t->val;
                      Trail(s);
                      goto OK;
                    }
                    else goto KO;

      case fun_tag: if (s->val==t->val)
                    { goto OK;
                    }
                    else
                    if (is_var(s))
                    { mkrefp(s,t);
                      Trail(s);
                      goto OK;
                    }
                    else  goto KO;


       // default:  goto KO;
    }
  OK: return(1);
  KO: return(0);
}

static inline
int unify_static_1(register cell_t *s, register cell_t *t)
{ s=deref(s);

  debut:
  if (s==t) goto OK;			// same object

  switch(get_tag(t))
    { case ref_tag: t=t->celp;
		    goto debut;
      case var_tag: mkrefp(t,s);
                    Trail(t);
                    goto OK;

      case ato_tag: if (is_var(s))
                    { mkrefp(s,t);
                      Trail(s);
                      goto OK;
                    }
                    else goto KO;

      case int_tag: if (s->val==t->val) goto OK;
                    if (is_var(s))
                    { s->val=t->val;
                      Trail(s);
                      goto OK;
                    }
                    else goto KO;

      case fun_tag: if (s->val==t->val)
                    { int n=get_arity(t);
		      for (;n>1;n--)
		        if (!unify_static_2(++s,++t))
                          goto KO;

                       s=deref(s+1);
                       t=t+1;
                       goto debut;
                    }
                    else
                    if (is_var(s))
                    { mkrefp(s,t);
                      Trail(s);
                      goto OK;
                    }
                    else  goto KO;


       default:  goto KO;
    }
  OK: return(1);
  KO: return(0);
}


static inline
int try_unify_static(term_t s, term_t t)
{ tr_t *tr=TP;
  int r;

  r=unify_static_1(s,t);
  reset(tr);

  return(r);
}


#define	SimpleHash(Val,Size)	((Val>>GC_BITS)%Size)

#define HashFromKey(Key,Fail)					\
({ __label__ debut;						\
   hash_t h;							\
								\
   debut:							\
   switch(get_tag(Key))						\
   { case ref_tag: Key=Key->celp;				\
                   goto debut;					\
     case ato_tag:						\
     case int_tag:						\
     case fun_tag: h=SimpleHash(Key->val,hash_recs_size);	\
                   break;					\
     default:      Fail;					\
   }								\
   h;								\
})								\


inline static
recl_t lookup_recl__old(cell_t *key)
{ hash_t h;
  recl_t rl;

  h=HashFromKey(key,return(0));

  for (rl=records[h]; rl!=0; rl=rl->next)
     if (key->val==rl->key.val) return(rl);	// find rec_list.

  rl=NEW(*rl);			// else create new flag
  rl->key=*key;			// with this key.
  rl->first=0;
  rl->last=0;
  rl->next=records[h];		// insert this recl in the table
  records[h]=rl;
  return(rl);
}
          
inline static
recl_t lookup_recl__(cell_t *key, int h)
{ recl_t rl;

  for (rl=records[h]; rl!=0; rl=rl->next)
     if (key->val==rl->key.val) // find rec_list.
       return(rl);

  return(0);
}
          
inline static
recl_t add_recl__(cell_t *key, int h)
{ recl_t rl;

  rl=NEW(*rl);			// create new recl
  rl->key=*key;			// with this key.
  rl->first=0;
  rl->last=0;
  rl->next=records[h];		// insert this recl in the table
  records[h]=rl;
  return(rl);
}
          
inline static
recl_t lookup_recl(term_t key, int h)
{ recl_t rl;

  if (!(rl=lookup_recl__(key,h)))
    rl=add_recl__(key,h);

  return(rl);
}

static
int pl_recordaz(cell_t *key, cell_t *term, cell_t *ref, int az)
{ recl_t rl;
  rec_t  r;
  hash_t h;
 
  h=HashFromKey(key,PL_warning("record%c/3 : illegal key", az));
  rl=lookup_recl(key,h);

  r=copy_to_heap(term);

  try(PL_unify_integer(ref, ((typeof(SHP)) r) - ((typeof(SHP)) 0x14000000) ) );

  if (rl->first==0 || rl->last==0)	// if rlist is empty
    { rl->last=rl->first=r;
      r->list=rl;
      r->next=0;
    }
  else
  if (az=='a')		// was recorda
    { r->next=rl->first;
      r->list=rl;
      rl->first=r;
    }
  else	// az='z'	// was recordz
    { r->list=rl;
      r->next=0;
      rl->last->next=r;
      rl->last=r;
    }

  succeed;
}

int pl_recorda(cell_t *k, cell_t *t, cell_t *ref)
{ return(pl_recordaz(k,t,ref,'a')); }

int pl_recorda_2(cell_t *k, cell_t *t)
{ term_t ref=PL_new_term_ref();
  return(pl_recordaz(k,t,ref,'a'));
}

int pl_recordz(cell_t *k, cell_t *t, cell_t *ref)
{ return(pl_recordaz(k,t,ref,'z')); }

int pl_recordz_2(cell_t *k, cell_t *t)
{ term_t ref=PL_new_term_ref();
  return(pl_recordaz(k,t,ref,'z'));
}

int pl_recorded(cell_t *key, cell_t *term, cell_t *ref, control_t ctrl)
{ recl_t rl;
  rec_t  r, *ctxt;
  hash_t h;

  switch(GetCtrl(ctrl))
  { case FIRST_CALL:
	ctxt=AllocCtxt(rec_t);
        h=HashFromKey(key,PL_warning("recorded/3 : illegal key"));

	if (!(rl=lookup_recl__(key,h)))
	  fail;
	else
	  r=rl->first;
	break;
    case NEXT_CALL:
	ctxt=GetCtxt(ctrl);
	r=*ctxt;
	break;
    default:
	fail;
  }

  for (;r;r=r->next)
  { if (try_unify_static(r->term,term))
    { mark_t m;
      Mark(m);

      if (unify(term,copy_to_global(r)) &&
          PL_unify_integer(ref, ((typeof(SHP)) r) - (SH_STK) ) )
        { *ctxt=r->next;
          retry;
        }
      else
      Undo(m);
    }
  }

  fail;
}

int pl_recorded_2(cell_t *key, cell_t *term, control_t ctrl)
{ term_t ref=PL_new_term_ref();
  return(pl_recorded(key,term,ref,ctrl));
}

 
int pl_current_key(cell_t *c, control_t ctrl)
{ recl_t recl;
  hash_t h;
  struct { hash_t hash; recl_t recl; } *ctxt;

  switch(GetCtrl(ctrl))
  { case FIRST_CALL:
	ctxt=AllocCtxt(*ctxt);
	h=0;
	recl=records[0];
	break;
    case NEXT_CALL:
	ctxt=GetCtxt(ctrl);
	h=ctxt->hash;
	recl=ctxt->recl;
	break;
    default:
	fail;
  }

  for (;h<hash_recs_size; recl=records[++h])
    for (;recl; recl=recl->next)
      if (unify_key(c,&(recl->key))) 	// FIXME : c is instantiated or variable
        { ctxt->hash=h;
          ctxt->recl=recl->next;
          retry;
        }

  fail;
}


/**********************************************************************/
/* Stuff for erase/1                                                  */
/**********************************************************************/

inline static
void free_record(rec_t r)
{ (void) r; }		// FIXME : put it in the free list ??


inline static 
int Erase_rec(rec_t rec)
{ rec_t prev;
  recl_t rl;

  rl=rec->list;

  if (rl->first==rec)
  { rl->first=rec->next;
    if (rec->next==0) rl->last=0;
    free_record(rec);
    succeed;
  }

  for (prev=rl->first; prev; prev=prev->next)
  { if (prev->next!=rec) continue;

    prev->next=rec->next;
    if (rec->next==0) rl->last=prev;
    free_record(rec);
    succeed;
  }
  PL_warning("erase/1 : illegal reference\n");

}

int pl_erase(cell_t *ref)
{ rec_t rec;

  Deref(ref);
  if (!is_intg(ref)) 
    PL_warning("erase/1 : illegal reference\n");

  rec=(rec_t) (SH_STK+get_intg(ref));
  return(Erase_rec(rec));
}

/**********************************************************************/
/* Specialized predicates                                             */
/**********************************************************************/

int pl_recorded_all(cell_t *key, cell_t *list)
{ recl_t rl;
  rec_t  r;
  hash_t h;
  term_t head,tail;


  h=HashFromKey(key,PL_warning("$recorded_all/3 : illegal key"));

  if ((rl=lookup_recl__(key,h)))
    r=rl->first;
  else
    r=0;

  head=tail=HP++;
  for (;r;r=r->next)
  { term_t c;

    c=copy_to_global(r);
    HP[0].val=__cons();
    HP[1].celp=c;
    tail->celp=HP;
    tail=HP+2;
    HP+=3;
  }
  tail->val=__nil();

  return(unify(head,list));
}

int pl_erase_records(term_t key)
{ rec_t r;
  recl_t rl;
  hash_t h;

  h=HashFromKey(key,PL_warning("$erase_records/1 : illegal key\n"));

  if (!(rl=lookup_recl__(key,h)))	// no records with this key
    succeed;

  rl->first=0;	// empty the recl.
  rl->last=0;

  for (r=rl->first; r; r=r->next)
    free_record(r);

  succeed;
}

/**********************************************************************/
/* Stuff for findall, bagof                                           */
/**********************************************************************/

static rec_t findall_recs=0;

int pl_findall_record(term_t t)
{ rec_t b;

  b=copy_to_heap(t);
  b->next=findall_recs;
  findall_recs=b;

  succeed;
}

static
void freeAssoc(rec_t prev, rec_t a) 
{ if (!prev)
    findall_recs = a->next;
  else
    prev->next = a->next;

  free_record(a);
}

int pl_findall_collect(term_t bag)
{ term_t list;	/* list to construct */
  term_t tmp;
  rec_t a, next;
  rec_t prev = 0;
  
  if (!(a = findall_recs)) fail;

  // PL_put_nil(list);
  list=(term_t) ATOM(nil);
					/* get variable term on global stack */
  for(next = a->next; next; a = next, next = a->next )
  { if (a->term->val==__atom(ATOM(_mark)))
      break;

    tmp=copy_to_global(a);
    tmp=tmp+2;
    HP[0].val=__cons();
    HP[1].celp=tmp;
    HP[2].celp=list;
    list=HP;
    HP+=3;
    freeAssoc(prev, a);
  }

  return PL_unify(bag, list);
}


/**********************************************************************/
/* Stuff for copy_term                                                */
/**********************************************************************/

// copy_term/2 stuff
// ressemble to Copy2Heap but much simpler
// optimized for ground term
// ( can be ineficient on deep unground tree 
//   since the ground test will be redone at each node ).
static
int CopyTerm(cell_t *addr, cell_t *c)
{ debut:
  switch(get_tag(c))
  { case ref_tag: c=c->celp;
                  goto debut;
    case var_tag: if ((c>=base) && (c<HP)) // ref to the copy
		    addr->celp=c;
                  else                     // new var
                  { if (addr)
                      addr->val=(var_tag<<29);
                    else 
                      addr=new_var();
                    c->celp=addr;
                    *TP++=c;
                  }
                  break;
    case ato_tag: if (!addr) addr=HP++;
                  addr->celp=c;
                  break;
    case int_tag: if (!addr) addr=HP++;
                  addr->val=c->val;
                  break;
    case fun_tag: if (pl_ground(c))
                  { if (addr)
                      addr->celp=c;
                    else
                    { HP->celp=c;
                      HP++;
                    }
                    break;
                  }
                  else
                  { int n=get_arity(c);
                    if (!addr)
                    { addr=HP;
                      HP+=(n+1);  
                    }
                    else
                    { addr=addr->celp=HP;
                      HP+=(n+1);
                    }

                    addr->val=c->val;
                    for (;n>1;n--)
                      if (!CopyTerm(++addr,++c))
                        fail;

                    ++addr; ++c;
                    goto debut;
                  }
    // default:      fail;
  }
  succeed;
}


int pl_copy_term(term_t src, term_t copy)
{ int r;
  tr_t *tp=TP;

  base=HP;
  r=CopyTerm(0, src);
  reset(tp);

  if (r)
    return(unify(base,copy));

  fail;
}


