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


struct flag {
	union cell key;			// Only atom, integer or first cell of a functor.
	union cell val;			// Only atom or integer (or float).
	struct flag *next;
};

#define hash_flags_size	256

static struct flag *flags[hash_flags_size];

inline static struct flag *lookup_flag(union cell *key)
{
	hash_t h;
	struct flag *f;

debut:
	switch (get_tag(key))		// get the hash value if the key is OK
	{
	case ref_tag:
		key = key->celp;
		goto debut;
	case ato_tag:
	case fun_tag:
		h = key->val >> GC_BITS;
		break;
	case int_tag:
		h = key->val;
		break;
	case flt_tag:
	default:
		return 0;
	}
	h = h % hash_flags_size;

	for (f = flags[h]; f != 0; f = f->next)
		if (key->val == f->key.val)
			return f;	// find flag.

	f = NEW(*f);			// else create new flag
	f->key = *key;			// with this key.
	f->next = flags[h];		// insert this flag in the table
	flags[h] = f;
	f->val.val = __intg(0);		// init the value with 0
	return f;
}

int pl_flag(union cell *key, union cell *old, union cell *new)
{
	struct flag *f;
	union cell *tmp;
	long n;

	f = lookup_flag(key);
	if (!f)
		PL_warning("flag/3: illegal key");

	tmp = PL_new_term_ref();
	*tmp = f->val;
	if (!pl_unify(old, tmp))
		fail;

	new = deref(new);
	if (is_atom(new)) {
		f->val.celp = new;
		succeed;
	} else if (PL_eval_(new, &n)) {
		f->val.val = __intg(n);
		succeed;
	} else
		PL_warning("flag/3: value should be an atom, integer or expression");
}

int pl_current_flag(union cell *c, enum control *ctrl)
{
	struct flag *flag;
	hash_t h;
	struct {
		hash_t hash;
		struct flag *flag;
	}     *ctxt;

	switch (GetCtrl(ctrl)) {
	case FIRST_CALL:
		ctxt = AllocCtxt(*ctxt);
		h = 0;
		flag = flags[h];
		break;
	case NEXT_CALL:
		ctxt = GetCtxt(ctrl);
		h = ctxt->hash;
		flag = ctxt->flag;
		break;
	default:
		fail;
	}

	for (; h < hash_flags_size; flag = flags[++h])
		for (; flag; flag = flag->next)
			if (PL_unify_key(c, &(flag->key)))	// FIXME : separe c is instantiated/variable ?
			{
				ctxt->hash = h;
				ctxt->flag = flag->next;
				retry;
			}

	fail;
}
