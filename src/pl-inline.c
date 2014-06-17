/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "pl.h"

#include "fli.h"
#include "pl-trad.h"
#include "pl-atom.h"
#include "pl-buffer.h"
#include "pl-inline.h"
#include "pl-os.h"
#include "pl-pred.h"
#include "pl-prims.h"
#include "pl-string.h"

#include <stdio.h>
union cell *deref_dbg(union cell * addr)
{
	union cell *p = addr;

	fprintf(stderr, "deref: p =      %p\n", p);
	while (p->tag_val.tag == ref_tag) {
		p = p->celp;
		fprintf(stderr, "     : p =      %p\n", p);
	}

	return (p);
}

union cell *new_flt(double r)
{
	typeof(HP) old_HP = HP;

	HP->val = MK_TAG(flt_tag);
	get_flt(HP) = r;
	HP += 3;
	return (old_HP);
}
