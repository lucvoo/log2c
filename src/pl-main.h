/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_MAIN_H_
#define PL_MAIN_H_

#define __MAIN__

pl_stack_t  *STK=0;
cell_t    *H_STK=0;
cell_t   *SH_STK=0;
tr_t	 *TR_STK=0;

void *PC=0;

#ifndef	SP_IN_REG
pl_stack_t *SP=0;
#endif
#ifndef	FP_IN_REG
pl_stack_t *FP=0;
#endif
#ifndef	HP_IN_REG
cell_t  *HP=0;
#endif
#ifndef	BTP_IN_REG
pl_stack_t  *BTP=0;
#endif
#ifndef	TP_IN_REG
tr_t  *TP=0;
#endif

cell_t  *SHP=0;



#ifdef	INTERACTIVE
int PL_nbr_fv;
#endif

#include "pl-inline.h"
#include "pl-pred.h"
#include "pl-trad.h"

extern void module__user(void);

#endif	// PL_MAIN_H_
