/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_PRED_H_
#define PL_PRED_H_

#include "pl.h"
#include "fli.h"
#include <limits.h>
 
extern int unify(register cell_t *d1, register cell_t *d2);
extern void PL_write_binding(void);
extern int PL_next_goal(void);
extern void PL_print_time(void);
extern int PL_eval_arith(register cell_t *c);
extern int PL_eval_arith_(register cell_t *c, int *n);
extern int PL_eval_(cell_t *c, int *n);
extern int PL_do_number_vars(cell_t *c);
extern int PL_can_unify(cell_t *, cell_t *);
extern int PL_not_unify(cell_t *, cell_t *);


int	Sprintf(const char *, ...) __attribute__((format (printf, 1, 2)));
int	Sprintf_err(const char *, ...) __attribute__((format (printf, 1, 2)));
int	PL_write(pl_stream S, term_t);



#ifdef TIME_OF_DAY
void PL_GetTime(struct timeval *tv);
#else
void PL_GetTime(time__t *t);
#endif


#endif	// PL_PRED_H_
