/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef	PL_PRED_H_
#define PL_PRED_H_

#include "pl.h"
#include "fli.h"
#include <limits.h>

extern void PL_write_binding(void);
extern void PL_print_time(void);
extern int PL_eval_arith(register union cell *c);
extern int PL_eval_arith_(register union cell *c, long *n);
extern int PL_eval_(union cell *c, long *n);
extern int PL_do_number_vars(union cell *c);
extern int PL_can_unify(union cell *, union cell *);
extern int PL_not_unify(union cell *, union cell *);

/* pl-proc.c */
void *PL_call(union cell *closure, int extra, union cell **args);
void *PL_apply(union cell *closure, union cell *list);

/* pl-Sprintf.c */
int Sprintf(const char *, ...) __attribute__ ((format(printf, 1, 2)));
int Sprintf_err(const char *, ...) __attribute__ ((format(printf, 1, 2)));

#endif
