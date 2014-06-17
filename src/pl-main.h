/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef	PL_MAIN_H_
#define PL_MAIN_H_

#define __MAIN__

union pl_stack*STK = 0;
union cell *H_STK = 0;
union cell *SH_STK = 0;
union cell **TR_STK = 0;

// void *PC=0;

#ifndef	HWREG_SP
union pl_stack*SP = 0;
#endif
#ifndef	HWREG_FP
union pl_stack*FP = 0;
#endif
#ifndef	HWREG_HP
union cell *HP = 0;
#endif
#ifndef	HWREG_BTP
union pl_stack*BTP = 0;
#endif
#ifndef	HWREG_TP
union cell **TP = 0;
#endif

union cell *SHP = 0;

#include "pl-inline.h"
#include "pl-pred.h"
#include "pl-trad.h"

extern void module__user(void);

#endif
