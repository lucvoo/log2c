/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef PL_FUN_H_
#define PL_FUN_H_

fun_t PL_lookup_fun(atom_t functor, int arity);
int pl_current_functor(cell_t *f, cell_t *n, control_t ctrl);

#endif	// PL_FUN_H_
