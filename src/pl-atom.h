/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_ATOM_H_
#define	PL_ATOM_H_

atom_t PL_lookup_atom(const char *s) ;
int pl_current_atom(cell_t *c, control_t ctrl);

#endif	// PL_ATOM_H_
