/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_PRIMS_H_
#define PL_PRIMS_H_

#include "pl.h"
#include "pl-inline.h"
#include "fli.h"

/* pl-proc.c */
void *PL_call(term_t closure, int extra, term_t *args);
void *PL_apply(term_t closure, term_t list);

/* pl-prims.c */
int PL_lengthList(term_t l);
int PL_unify_list_codes(term_t l, const char *s);
int pl_std_cmp(term_t t1, term_t t2);
int pl_std_eq(term_t t1, term_t t2);
int pl_struct_eq(cell_t *t1, cell_t *t2);


#endif	// PL_PRIMS_H_
