/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef	PL_OP_H_
#define	PL_OP_H_

#define	OP_PREFIX	1
#define	OP_POSTFIX	2
#define	OP_INFIX	3

#define OP_FX		((OP_PREFIX<<2) | 0)
#define OP_FY		((OP_PREFIX<<2) | 1)
#define OP_XF		((OP_POSTFIX<<2) | 0)
#define OP_YF		((OP_POSTFIX<<2) | 1)
#define OP_XFX		((OP_INFIX<<2) | 0)
#define OP_XFY		((OP_INFIX<<2) | 1)
#define OP_YFX		((OP_INFIX<<2) | 2)
#define OP_YFY		((OP_INFIX<<2) | 3)

int PL_is_op(int fix, struct atom *operator, int *type, int *prec);
int PL_can_be_op(struct atom *operator);

#endif
