/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-hash.h"
#include "pl-pred.h"
#include "pl-fli.h"


inline static void rtrail(union cell *ref)
{
	HP->celp = ref;
	HP++;
}

struct record {
	struct reclist *list;
	struct record *next;
	int size;
	union cell term[0];
};
struct reclist {
	union cell key;
	struct record *first;
	struct record *last;
	struct reclist *next;
};

#define hash_recs_size	256
static struct reclist *records[hash_recs_size];

static union cell *Copy2Heap(union cell *addr, union cell *c, const union cell *base)
{
debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case var_tag:
		if ((c >= base) && (c < SHP))	// ref to the copy
		{
			addr->celp = c;
			return c;
		} else			// new var
		if (addr)		// write to a fun object.
		{
			addr->val = MK_TAG(var_tag);
			c->celp = addr;
			trail(c);
			return 0;
		} else			// single object
		{
			addr = new_var();
			c->celp = addr;
			trail(c);
			return addr;
		}
	case ato_tag:
		if (!addr)
			addr = NEW(union cell);
		addr->celp = c;
		return addr;
	case int_tag:
		if (!addr)
			addr = NEW(union cell);
		addr->val = c->val;
		return addr;
	case fun_tag:{
			int n = get_arity(c);
			if (!addr)
				addr = NEW_(union cell, n + 1);
			else {
				addr->celp = NEW_(union cell, n + 1);
				addr = addr->celp;
			}

			addr->val = c->val;
			for (; n > 0; n--)
				Copy2Heap(addr + n, c + n, base);

			return addr;
		}
	}
	return 0;
}

inline static struct record *copy_to_heap(union cell *c)
{
	union cell *base;
	struct record *record;
	union cell **tr;

	tr = TP;
	record = NEW(*record);
	base = SHP;			// == record->term == record+1;
	Copy2Heap(0, c, base);

	record->size = SHP - base;	// == SHP-record->term
	reset(tr);
	return record;
}

inline static union cell *copy_to_global(struct record *record)
{
	int n, i;
	union cell *c;

	c = record->term;
	n = record->size;
	for (i = 0; i < n; i++) {
		if (is_ref(c + i) && c[i].celp >= c && c[i].celp < c + n)
			HP[i].celp = (c[i].celp) + (HP - c);
		else
			HP[i].val = c[i].val;
	}

	c = HP;
	HP += record->size;

	return c;
}

/**********************************************************************/
/* unify_static == unify in the record without first copy the term    */
/**********************************************************************/

#define Trail(T)	*TP++=T

static inline int unify_static_2(union cell *s, union cell *t)
{
	s = deref(s);

debut:
	if (s == t)
		goto OK;		// same object

	switch (get_tag(t)) {
	case ref_tag:
		t = t->celp;
		goto debut;
	case var_tag:
		mkrefp(t, s);
		Trail(t);
		goto OK;

	case ato_tag:
		if (is_var(s)) {
			mkrefp(s, t);
			Trail(s);
			goto OK;
		} else
			goto KO;

	case int_tag:
		if (s->val == t->val)
			goto OK;
		if (is_var(s)) {
			s->val = t->val;
			Trail(s);
			goto OK;
		} else
			goto KO;

	case fun_tag:
		if (s->val == t->val) {
			goto OK;
		} else if (is_var(s)) {
			mkrefp(s, t);
			Trail(s);
			goto OK;
		} else
			goto KO;

		// default:  goto KO;
	}
OK:	return 1;
KO:	return 0;
}

static inline int unify_static_1(union cell *s, union cell *t)
{
	s = deref(s);

debut:
	if (s == t)
		goto OK;		// same object

	switch (get_tag(t)) {
	case ref_tag:
		t = t->celp;
		goto debut;
	case var_tag:
		mkrefp(t, s);
		Trail(t);
		goto OK;

	case ato_tag:
		if (is_var(s)) {
			mkrefp(s, t);
			Trail(s);
			goto OK;
		} else
			goto KO;

	case int_tag:
		if (s->val == t->val)
			goto OK;
		if (is_var(s)) {
			s->val = t->val;
			Trail(s);
			goto OK;
		} else
			goto KO;

	case fun_tag:
		if (s->val == t->val) {
			int n = get_arity(t);
			for (; n > 1; n--)
				if (!unify_static_2(++s, ++t))
					goto KO;

			s = deref(s + 1);
			t = t + 1;
			goto debut;
		} else if (is_var(s)) {
			mkrefp(s, t);
			Trail(s);
			goto OK;
		} else
			goto KO;

	default:
		goto KO;
	}
OK:	return 1;
KO:	return 0;
}

static inline int try_unify_static(union cell *s, union cell *t)
{
	union cell **tr = TP;
	int r;

	r = unify_static_1(s, t);
	reset(tr);

	return r;
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


inline static struct reclist *lookup_recl__old(union cell *key)
{
	hash_t h;
	struct reclist *rl;

	h = HashFromKey(key, return 0);

	for (rl = records[h]; rl != 0; rl = rl->next)
		if (key->val == rl->key.val)
			return rl;	// find rec_list.

	rl = NEW(*rl);			// else create new flag
	rl->key = *key;			// with this key.
	rl->first = 0;
	rl->last = 0;
	rl->next = records[h];		// insert this recl in the table
	records[h] = rl;
	return rl;
}

inline static struct reclist *lookup_recl__(union cell *key, int h)
{
	struct reclist *rl;

	for (rl = records[h]; rl != 0; rl = rl->next)
		if (key->val == rl->key.val)	// find rec_list.
			return rl;

	return 0;
}

inline static struct reclist *add_recl__(union cell *key, int h)
{
	struct reclist *rl;

	rl = NEW(*rl);			// create new recl
	rl->key = *key;			// with this key.
	rl->first = 0;
	rl->last = 0;
	rl->next = records[h];		// insert this recl in the table
	records[h] = rl;
	return rl;
}

inline static struct reclist *lookup_recl(union cell *key, int h)
{
	struct reclist *rl;

	if (!(rl = lookup_recl__(key, h)))
		rl = add_recl__(key, h);

	return rl;
}

static int pl_recordaz(union cell *key, union cell *term, union cell *ref, int az)
{
	struct reclist *rl;
	struct record *r;
	hash_t h;

	h = HashFromKey(key, PL_warning("record%c/3 : illegal key", az));
	rl = lookup_recl(key, h);

	r = copy_to_heap(term);

	try(PL_unify_intg(ref, ((typeof(SHP)) r) - ((typeof(SHP)) 0x14000000)));

	if (rl->first == 0 || rl->last == 0)	// if rlist is empty
	{
		rl->last = rl->first = r;
		r->list = rl;
		r->next = 0;
	} else if (az == 'a')		// was recorda
	{
		r->next = rl->first;
		r->list = rl;
		rl->first = r;
	} else				// az='z'       // was recordz
	{
		r->list = rl;
		r->next = 0;
		rl->last->next = r;
		rl->last = r;
	}

	succeed;
}

int pl_recorda(union cell *k, union cell *t, union cell *ref)
{
	return pl_recordaz(k, t, ref, 'a');
}

int pl_recorda_2(union cell *k, union cell *t)
{
	union cell *ref = PL_new_term_ref();
	return pl_recordaz(k, t, ref, 'a');
}

int pl_recordz(union cell *k, union cell *t, union cell *ref)
{
	return pl_recordaz(k, t, ref, 'z');
}

int pl_recordz_2(union cell *k, union cell *t)
{
	union cell *ref = PL_new_term_ref();
	return pl_recordaz(k, t, ref, 'z');
}

int pl_recorded(union cell *key, union cell *term, union cell *ref, enum control *ctrl)
{
	struct reclist *rl;
	struct record *r, **ctxt;
	hash_t h;

	switch (GetCtrl(ctrl)) {
	case FIRST_CALL:
		ctxt = AllocCtxt(struct record *);
		h = HashFromKey(key, PL_warning("recorded/3 : illegal key"));

		if (!(rl = lookup_recl__(key, h)))
			fail;
		else
			r = rl->first;
		break;
	case NEXT_CALL:
		ctxt = GetCtxt(ctrl);
		r = *ctxt;
		break;
	default:
		fail;
	}

	for (; r; r = r->next) {
		if (try_unify_static(r->term, term)) {
			struct mark m;
			Mark(m);

			if (pl_unify(term, copy_to_global(r)) &&
			    PL_unify_intg(ref, ((typeof(SHP)) r) - (SH_STK))) {
				*ctxt = r->next;
				retry;
			} else
				Undo(m);
		}
	}

	fail;
}

int pl_recorded_2(union cell *key, union cell *term, enum control *ctrl)
{
	union cell *ref = PL_new_term_ref();
	return pl_recorded(key, term, ref, ctrl);
}

int pl_current_key(union cell *c, enum control *ctrl)
{
	struct reclist *recl;
	hash_t h;
	struct {
		hash_t hash;
		struct reclist *recl;
	}     *ctxt;

	switch (GetCtrl(ctrl)) {
	case FIRST_CALL:
		ctxt = AllocCtxt(*ctxt);
		h = 0;
		recl = records[0];
		break;
	case NEXT_CALL:
		ctxt = GetCtxt(ctrl);
		h = ctxt->hash;
		recl = ctxt->recl;
		break;
	default:
		fail;
	}

	for (; h < hash_recs_size; recl = records[++h])
		for (; recl; recl = recl->next)
			if (PL_unify_key(c, &(recl->key)))	// FIXME : c is instantiated or variable
			{
				ctxt->hash = h;
				ctxt->recl = recl->next;
				retry;
			}

	fail;
}

/**********************************************************************/
/* Stuff for erase/1                                                  */
/**********************************************************************/

inline static void free_record(struct record *r)
{
	(void)r;
}					// FIXME : put it in the free list ??

inline static int Erase_rec(struct record *rec)
{
	struct record *prev;
	struct reclist *rl;

	rl = rec->list;

	if (rl->first == rec) {
		rl->first = rec->next;
		if (rec->next == 0)
			rl->last = 0;
		free_record(rec);
		succeed;
	}

	for (prev = rl->first; prev; prev = prev->next) {
		if (prev->next != rec)
			continue;

		prev->next = rec->next;
		if (rec->next == 0)
			rl->last = prev;
		free_record(rec);
		succeed;
	}
	PL_warning("erase/1 : illegal reference\n");

}

int pl_erase(union cell *ref)
{
	struct record *rec;

	Deref(ref);
	if (!is_intg(ref))
		PL_warning("erase/1 : illegal reference\n");

	rec = (struct record *) (SH_STK + get_intg(ref));
	return Erase_rec(rec);
}

/**********************************************************************/
/* Specialized predicates                                             */
/**********************************************************************/

int pl_recorded_all(union cell *key, union cell *list)
{
	struct reclist *rl;
	struct record *r;
	hash_t h;
	union cell *head, *tail;

	h = HashFromKey(key, PL_warning("$recorded_all/3 : illegal key"));

	if ((rl = lookup_recl__(key, h)))
		r = rl->first;
	else
		r = 0;

	head = tail = HP++;
	for (; r; r = r->next) {
		union cell *c;

		c = copy_to_global(r);
		HP[0].val = __cons();
		HP[1].celp = c;
		tail->celp = HP;
		tail = HP + 2;
		HP += 3;
	}
	tail->val = __nil();

	return pl_unify(head, list);
}

int pl_erase_records(union cell *key)
{
	struct record *r;
	struct reclist *rl;
	hash_t h;

	h = HashFromKey(key, PL_warning("$erase_records/1 : illegal key\n"));

	if (!(rl = lookup_recl__(key, h)))	// no records with this key
		succeed;

	rl->first = 0;			// empty the recl.
	rl->last = 0;

	for (r = rl->first; r; r = r->next)
		free_record(r);

	succeed;
}

/**********************************************************************/
/* Stuff for findall, bagof                                           */
/**********************************************************************/

static struct record *findall_recs = 0;

int pl_findall_record(union cell *t)
{
	struct record *b;

	b = copy_to_heap(t);
	b->next = findall_recs;
	findall_recs = b;

	succeed;
}

static void freeAssoc(struct record *prev, struct record *a)
{
	if (!prev)
		findall_recs = a->next;
	else
		prev->next = a->next;

	free_record(a);
}

int pl_findall_collect(union cell *bag)
{
	union cell *list;			/* list to construct */
	union cell *tmp;
	struct record *a, *next;
	struct record *prev = 0;

	if (!(a = findall_recs))
		fail;

	// PL_put_nil(list);
	list = (union cell *) ATOM(nil);
	/* get variable term on global stack */
	for (next = a->next; next; a = next, next = a->next) {
		if (a->term->val == __atom(ATOM(_mark)))
			break;

		tmp = copy_to_global(a);
		tmp = tmp + 2;
		HP[0].val = __cons();
		HP[1].celp = tmp;
		HP[2].celp = list;
		list = HP;
		HP += 3;
		freeAssoc(prev, a);
	}

	return pl_unify(bag, list);
}

/**********************************************************************/
/* Stuff for copy_term                                                */
/**********************************************************************/

// copy_term/2 stuff
// ressemble to Copy2Heap but much simpler
// optimized for ground term
// ( can be ineficient on deep unground tree
//   since the ground test will be redone at each node ).
static int CopyTerm(union cell *addr, union cell *c, const union cell *base)
{
debut:
	switch (get_tag(c)) {
	case ref_tag:
		c = c->celp;
		goto debut;
	case var_tag:
		if ((c >= base) && (c < HP))	// ref to the copy
			addr->celp = c;
		else			// new var
		{
			if (addr)
				addr->val = MK_TAG(var_tag);
			else
				addr = new_var();
			c->celp = addr;
			*TP++ = c;
		}
		break;
	case ato_tag:
		if (!addr)
			addr = HP++;
		addr->celp = c;
		break;
	case int_tag:
		if (!addr)
			addr = HP++;
		addr->val = c->val;
		break;
	case fun_tag:
		if (pl_ground(c)) {
			if (addr)
				addr->celp = c;
			else {
				HP->celp = c;
				HP++;
			}
			break;
		} else {
			int n = get_arity(c);
			if (!addr) {
				addr = HP;
				HP += (n + 1);
			} else {
				addr = addr->celp = HP;
				HP += (n + 1);
			}

			addr->val = c->val;
			for (; n > 1; n--)
				if (!CopyTerm(++addr, ++c, base))
					fail;

			++addr;
			++c;
			goto debut;
		}
		// default:      fail;
	}
	succeed;
}

int pl_copy_term(union cell *src, union cell *copy)
{
	union cell *base;
	int r;
	union cell **tp = TP;

	base = HP;
	r = CopyTerm(0, src, base);
	reset(tp);

	if (r)
		return pl_unify(base, copy);

	fail;
}
