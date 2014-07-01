/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-hash.h"
#include "pl-string.h"
#include "pl-atom.h"

inline static struct atom *add_atom(const char *s, hash_t H, hash_t h)
{
	struct atom *a;

	a = NEW(*a);
	a->cell.val = MK_CELL(ato_tag, a);
	a->name = s;
	a->hash = H;
	a->next = PL__atoms[h];
	PL__atoms[h] = a;
	PL__atoms_count++;

	return a;
}

struct atom *PL_new_atom(const char *s)
{
	hash_t h, H;
	struct atom *a;
	const char *copy;

	H = PL_hash_str(s);
	h = H % PL__atoms_hash_size;

	for (a = PL__atoms[h]; a != 0; a = a->next) {
		if (streq(s, a->name))
			return a;
	}

	copy = new_str(s);
	return add_atom(copy, H, h);
}

int pl_current_atom(union cell *c, enum control *ctrl)
{
	struct atom *atom;
	hash_t h;
	struct {
		hash_t hash;
		struct atom *atom;
	}     *ctxt;

	switch (GetCtrl(ctrl)) {
	case FIRST_CALL:
		if (PL_is_atom(c))
			succeed;
		if (!PL_is_var(c))
			fail;

		ctxt = AllocCtxt(*ctxt);
		h = 0;
		atom = PL__atoms[h];
		break;
	case NEXT_CALL:
		ctxt = GetCtxt(ctrl);
		h = ctxt->hash;
		atom = ctxt->atom;
		break;
	default:
		fail;
	}

	for (; h < PL__atoms_hash_size; atom = PL__atoms[++h])
		if (atom) {
			PL_unify_atom(c, atom);	// Always succeed since c is a var !
			ctxt->hash = h;
			ctxt->atom = atom->next;
			retry;
		}

	fail;
}
