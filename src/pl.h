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

extern cell_t  *SHP;
extern pl_stack_t    *STK;
extern cell_t   *H_STK;
extern tr_t    *TR_STK;
extern cell_t  *SH_STK;

extern void *PC;

extern int hash_atoms_size;	// must be a power of two for dynamic hashing
extern atom_t atoms[];
extern int hash_funs_size;	// must be a power of two for dynamic hashing
extern fun_t  funs[];

extern int nbr_fv;
extern const char *freevar[];

#include "ATOMS.h"

#endif	// PL_H_
