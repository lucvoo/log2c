/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_PRED_H_
#define PL_PRED_H_

#include "pl.h"
#include "fli.h"
#include <limits.h>
 
extern void	PL_write_binding(void);
extern int	PL_next_goal(void);
extern void	PL_print_time(void);
extern int	PL_eval_arith(register cell_t *c);
extern int	PL_eval_arith_(register cell_t *c, int *n);
extern int	PL_eval_(cell_t *c, int *n);
extern int	PL_do_number_vars(cell_t *c);
extern int	PL_can_unify(cell_t *, cell_t *);
extern int	PL_not_unify(cell_t *, cell_t *);

/* pl-proc.c */
void *PL_call(term_t closure, int extra, term_t *args);
void *PL_apply(term_t closure, term_t list);

/* pl-Sprintf.c */
int	Sprintf(const char *, ...) __attribute__((format (printf, 1, 2)));
int	Sprintf_err(const char *, ...) __attribute__((format (printf, 1, 2)));


#endif	// PL_PRED_H_
