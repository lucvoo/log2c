/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef  PL_STACK_H_
#define  PL_STACK_H_

struct pl_stack_stat {
	struct {
		int heap;
		int local;
		int trail;
		int sheap;
	} used, free;
};

int PL_get_stack_stat(struct pl_stack_stat *);

#endif
