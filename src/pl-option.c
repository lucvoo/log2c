/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-fli.h"
#include "pl-option.h"


static int map_values(const struct pl_option_map *map, const struct atom *name)
{
	for (; map->name; map++) {
		if (map->name == name)
			return map->val;
	}

	return -1;
}

int PL_scan_options(union cell *options, struct pl_option_spec *spec)
{
	union cell *list = deref(options);
	union cell *val;
	struct pl_option_spec *s = 0;

	for (list = deref(list);  is_cons(list); list = deref(list + 2)) {		// loop trough the options list
		union cell *e = deref(list+1);
		struct atom *name;
		int arity;

		if (PL_get_name_arity(e, &name, &arity)) {
			if (arity == 0) {
				val =  &ATOM(_true)->cell;
			} else if (arity == 1) {
				val = e + 1;
			} else if (arity == 2 && name == ATOM(unify)) {
				if (!(name = PL_get_atom(e + 1)))
					fail;
				val = e + 2;
			} else
				fail;
		} else
			fail;

		for (s = spec; s->name; s++) {
			if (s->name == name)
				break;
		}
		if (!s->name)
			fail;

		switch (s->type) {
		struct atom *a;
		long i;

		case OPT_BOOL:
			if (!(a = PL_get_atom(val)))
				fail;
			if (a == ATOM(_true) || a == ATOM(_on)) {
				i = 1;
			} else if (a == ATOM(_false) || a == ATOM(_off)) {
				i = 0;
			} else
				fail;
			*s->val.bool = i;
			break;
		case OPT_INTG:
			if (!PL_get_long(val, &i))
				fail;
			*s->val.intg = i;
			break;
		case OPT_ATOM:
			if (!(a = PL_get_atom(val)))
				fail;
			*s->val.atom = a;
			break;
		case OPT_ATOMS:
			name = PL_get_atom(val);
			if (!name)
				fail;
			if ((i = map_values(s->map, name)) == -1)
				fail;
			*s->val.intg = i;
			break;
		case OPT_TERM:
			*s->val.term = val;
			break;
		default:
			fail;
		}
	}

	return is_nil(list);
}
