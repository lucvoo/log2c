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

inline static struct flag *lookup_flag(union cell * key)
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

int pl_flag(union cell * key, union cell * old, union cell * new)
{
	struct flag *f;
	union cell *tmp;
	int n;

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

int pl_current_flag(union cell * c, enum control *ctrl)
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

/**********************************************************************/
/* flag2 : for internal use only ?                                    */
/**********************************************************************/

struct flag2 {
	union cell key1;			// Only atoms, int, functor
	union cell key2;
	union cell val;			// Only atom or integer (or float).
	struct flag2 *next;
};
// #define hash_flag_2_size     256
#define hash_flag_2_size	4

static struct flag2 *flag_2_tbl[hash_flag_2_size];

inline static struct flag2 *lookup_flag_2(union cell *key1, union cell *key2, int new)
{
	hash_t h, h1, h2;
	struct flag2 *f;

	key1 = deref(key1);
	key2 = deref(key2);

	if ((h1 = SimpleHashValue(key1)) && (h2 = SimpleHashValue(key2)))
		h = (h1 + h2) % hash_flag_2_size;
	else {				// FIXME : msg : illegal key
		return 0;
	}

	for (f = flag_2_tbl[h]; f; f = f->next)
		if ((key1->val == f->key1.val) && (key2->val == f->key2.val))
			return f;	// find flag.

	if (new) {
		f = NEW(*f);		// create new flag
		f->key1 = *key1;	// with these keys.
		f->key2 = *key2;
		f->next = flag_2_tbl[h];	// insert this flag in the table
		flag_2_tbl[h] = f;
		return f;
	} else
		return 0;		// inexistant flag
}

static int PL_flag_2(union cell *key1, union cell *key2, union cell *val)
{
	struct flag2 *f;

	if ((f = lookup_flag_2(key1, key2, 0)))
		return PL_unify_atomic(val, f->val);
	else
		fail;
}

int pl_flag_2(union cell *key1, union cell *key2, union cell *val, enum control *ctrl)
{
	struct flag2 *f;
	hash_t h;
	union cell **tr;
	struct {
		hash_t hash;
		struct flag2 *flag;
	}     *ctxt;

	key1 = deref(key1);
	key2 = deref(key2);

	if (!is_var(key1) && !is_var(key2))
		return PL_flag_2(key1, key2, val);

	switch (GetCtrl(ctrl)) {
	case FIRST_CALL:
		ctxt = AllocCtxt(*ctxt);
		h = 0;
		f = flag_2_tbl[h];
		break;
	case NEXT_CALL:
		ctxt = GetCtxt(ctrl);
		h = ctxt->hash;
		f = ctxt->flag;
		break;
	default:
		fail;
	}

	tr = TP;
	for (; h < hash_flag_2_size; f = flag_2_tbl[++h])
		for (; f; f = f->next)
			if (PL_unify_atomic(key1, f->key1) &&
			    PL_unify_atomic(key2, f->key2) && PL_unify_atomic(val, f->val)) {
				ctxt->hash = h;
				ctxt->flag = f->next;
				retry;
			} else
				reset(tr);

	fail;
}

static int PL_set_flag_2(union cell *key1, union cell *key2, union cell *val)
{
	struct flag2 *f;

	if ((f = lookup_flag_2(key1, key2, 1))) {
		val = deref(val);
		if (is_atom(val) || is_intg(val) || is_flt(val)) {
			f->val = *val;
			succeed;
		} else
			PL_warning("$set_flag2/3 : instantiation fault");

		succeed;
	} else
		fail;
}

int pl_set_flag_2(union cell *key1, union cell *key2, union cell *val)
{
	if (PL_set_flag_2(key1, key2, val))
		succeed;
	else
		PL_warning("$set_flag2: illegal key(s)");
}
