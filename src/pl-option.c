/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-fli.h"
#include "pl-option.h"

int PL_scan_options(union cell *options, struct pl_option_spec *spec)
{
	union cell *list = deref(options);
	union cell *val;
	struct pl_option_spec *s = 0;

	list = deref(list);
	while (is_cons(list))		// loop trough the options list
	{
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

		for (s = spec; s->name; s++)	// loop trough the option spec
		{
			if (s->name != name)
				continue;

			switch (s->type) {
			case OPT_BOOL:
				if (!(name = PL_get_atom(val)))
					fail;
				if (name == ATOM(_true) || name == ATOM(_on)) {
					*s->val.bool = 1;
				} else if (name == ATOM(_false) || name == ATOM(_off)) {
					*s->val.bool = 0;
				} else
					fail;
				break;
			case OPT_INTG:
				if (PL_get_long(val, s->val.intg))
					break;
				else
					fail;
			case OPT_ATOM:
				if ((*s->val.atom = PL_get_atom(val)))
					break;
				else
					fail;
			case OPT_TERM:
				*s->val.term = val;
				break;
			default:
				fail;
			}
			goto loop_list;
		}			// POST s->name==0
		fail;

loop_list:
		list = deref(list + 2);
	}

	return is_nil(list);
}
