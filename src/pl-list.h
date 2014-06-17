/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef	PL_LIST_H_
#define PL_LIST_H_

#include "pl-types.h"

int pl_memberchk(union cell *e, union cell *l);
int pl_length(union cell *t, union cell *l);

#endif
