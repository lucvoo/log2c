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
extern void write_binding(void);
extern int next_goal(void);
extern void print_time(void);
extern int eval_arith(register cell_t *c);
extern int eval_arith_(register cell_t *c, int *n);
extern int eval_(cell_t *c, int *n);
extern int do_number_vars(cell_t *c);
extern int can_unify(cell_t *, cell_t *);
extern int not_unify(cell_t *, cell_t *);


int	Sprintf(const char *, ...) __attribute__((format (printf, 1, 2)));
int	Sprintf_err(const char *, ...) __attribute__((format (printf, 1, 2)));
int	PL_write(pl_stream S, term_t);



#ifdef TIME_OF_DAY
void get_time(struct timeval *tv);
#else
void get_time(time__t *t);
#endif


#endif	// PL_PRED_H_
