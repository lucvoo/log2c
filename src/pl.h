/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef	PL_H_
#define PL_H_

#include "pl-config.h"
#include "pl-types.h"

#ifdef	HWREG_SP
register union pl_stack*SP asm(HWREG_SP);
#else
extern union pl_stack*SP;
#endif

#ifdef	HWREG_HP
register union cell *HP asm(HWREG_HP);
#else
extern union cell *HP;
#endif

#ifdef	HWREG_FP
register union pl_stack*FP asm(HWREG_FP);
#else
extern union pl_stack*FP;
#endif

#ifdef	HWREG_BTP
register union pl_stack*BTP asm(HWREG_BTP);
#else
extern union pl_stack*BTP;
#endif

#ifdef	HWREG_TP
register union cell **TP asm(HWREG_TP);
#else
extern union cell **TP;
#endif

extern union cell *SHP;
extern union pl_stack*STK;
extern union cell *H_STK;
extern union cell **TR_STK;
extern union cell *SH_STK;

#ifdef	HWREG_ARGS
extern union cell *PL_ARGS_[PL_MAX_ARGS];
register union cell **PL_ARGS asm(HWREG_ARGS);
#else
extern union cell *PL_ARGS[PL_MAX_ARGS];
#endif

// must be a power of two for dynamic hashing
extern int PL__atoms_hash_size;
extern int PL__atoms_count;
extern struct atom *PL__atoms[];

// must be a power of two for dynamic hashing
extern int PL__funs_hash_size;
extern int PL__funs_count;
extern struct functor *PL__funs[];

#include "ATOMS.h"

#endif
