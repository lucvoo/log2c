/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_PRIMS_H_
#define PL_PRIMS_H_

#include "pl.h"
#include "pl-inline.h"
#include "fli.h"

char *formatInteger(bool split, int div, int radix, bool small, long int n);
int GetSingleChar(void);

/* pl-proc.c */
void *PL_call(term_t closure, int extra, term_t *args);
void *PL_apply(term_t closure, term_t list);

/* pl-prims.c */
int lengthList(term_t l);
int pl_between(cell_t *low, cell_t *high, cell_t *n, control_t ctrl);
int pl_plus(cell_t *d1, cell_t *d2, cell_t *d3);
int pl_ground(cell_t *d);
int pl_unground(cell_t *d);
int free_variables(cell_t *c, int n);
int pl_free_variables(cell_t *t, cell_t *fv);
int PL_unify_list_codes(term_t l, const char *s);
int pl_atom_chars(term_t a, term_t list);
int pl_atom_codes(term_t a, term_t list);
int pl_atom_prefix(term_t atom, term_t prefix);
int pl_functor(term_t t, term_t f, term_t a);
int pl_univ(term_t t, term_t l);
int pl_std_cmp(term_t t1, term_t t2);
int pl_std_eq(term_t t1, term_t t2);
int pl_struct_eq(cell_t *t1, cell_t *t2);
int pl_int_to_atom2(term_t num, term_t atom);
int pl_int_to_atom3(term_t num, term_t base, term_t atom);
int pl_concat(term_t a1, term_t a2, term_t a3);
int pl_concat_atom2(term_t list, term_t atom);
int pl_halt(term_t stat);
int pl_arg(term_t n, term_t term, term_t arg);
int pl_setarg(term_t n, term_t term, term_t value);
char *formatInteger(bool split, int div, int radix, bool small, long int n);
int pl_hpjw(term_t str, term_t h_val);

/* pl-list.c */
int pl_is_list(term_t l);
int pl_proper_list(term_t l);
int pl_partial_list(term_t l);
int pl_memberchk(term_t e, term_t l);
int pl_length(term_t list, term_t l);
int pl_sort(term_t list, term_t sorted);
int pl_msort(term_t list, term_t sorted);


#endif	// PL_PRIMS_H_
