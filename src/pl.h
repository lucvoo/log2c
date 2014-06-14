/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_H_
#define PL_H_

#include "pl-config.h"
#include "pl-types.h"


#ifdef	HWREG_SP
register pl_stack_t *SP asm(HWREG_SP);
#else
extern pl_stack_t *SP;
#endif

#ifdef	HWREG_HP
register cell_t *HP asm(HWREG_HP);
#else
extern cell_t  *HP;
#endif

#ifdef	HWREG_FP
register pl_stack_t *FP asm(HWREG_FP);
#else
extern pl_stack_t *FP;
#endif

#ifdef	HWREG_BTP
register pl_stack_t *BTP asm(HWREG_BTP);
#else
extern pl_stack_t *BTP;
#endif

#ifdef	HWREG_TP
register tr_t *TP asm(HWREG_TP);
#else
extern tr_t *TP;
#endif

extern cell_t		*SHP;
extern pl_stack_t	*STK;
extern cell_t		*H_STK;
extern tr_t		*TR_STK;
extern cell_t		*SH_STK;


extern cell_t *PL_ARGS[PL_MAX_ARGS];


// must be a power of two for dynamic hashing
extern int	PL__atoms_hash_size;
extern int	PL__atoms_count;
extern atom_t	PL__atoms[];

// must be a power of two for dynamic hashing
extern int	PL__funs_hash_size;
extern int	PL__funs_count;
extern fun_t	PL__funs[];


#include "ATOMS.h"

#endif	// PL_H_
