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
	// union cell *head;
	union cell *val = PL_new_term_ref();
	struct pl_option_spec *s = 0;

	while (is_cons(list))		// loop trough the options list
	{
		struct atom *name;
		int arity;

		if (PL_get_name_arity(list + 1, &name, &arity)) {
			if (arity == 0) {
				PL_put_atom(val, ATOM(_true));
			} else if (arity == 1) {
				PL_get_arg(1, list + 1, val);
			} else if (arity == 2 && name == ATOM(unify)) {
				union cell *head = deref(list + 1);
				if (!(name = PL_get_atom(head + 1)))
					fail;
				val = head + 2;
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
				val = PL_new_term_ref();
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
