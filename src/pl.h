/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_H_
#define PL_H_

#include "pl-config.h"
#include "pl-types.h"


#ifdef	SP_IN_REG
register pl_stack_t *SP asm ("%ebx");
#else
extern pl_stack_t *SP;
#endif

#ifdef	HP_IN_REG
register cell_t *HP asm ("%edi");
#else
extern cell_t  *HP;
#endif

#ifdef	FP_IN_REG
register pl_stack_t *FP asm ("%esi");
#else
extern pl_stack_t *FP;
#endif

#ifdef	BTP_IN_REG
register pl_stack_t *BTP asm ("%ebx");
#else
extern pl_stack_t *BTP;
#endif

#ifdef	TP_IN_REG
register tr_t *TP asm ("%ecx");
#else
extern tr_t *TP;
#endif

extern cell_t		*SHP;
extern pl_stack_t	*STK;
extern cell_t		*H_STK;
extern tr_t		*TR_STK;
extern cell_t		*SH_STK;


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
