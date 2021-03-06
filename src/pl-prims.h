/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef	PL_PRIMS_H_
#define PL_PRIMS_H_

#include "pl.h"
#include "pl-inline.h"
#include "fli.h"

/* pl-prims.c */
int PL_lengthList(union cell *l);
int PL_list_tail(union cell *list, union cell **tail);
int PL_unify_list_codes(union cell *l, const char *s);
int pl_std_cmp(const union cell *t1, const union cell *t2);
int pl_std_eq(union cell *t1, union cell *t2);
int pl_struct_eq(union cell *t1, union cell *t2);

#endif
