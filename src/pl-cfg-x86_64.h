/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifdef    STACK_DEF
//         symbol  , address   , incr , max  , name
STACK_DEF(HEAP_STK, 0x100000000000, 1 << 30, 1 << 30, "heap")
	STACK_DEF(SHEAP_STK, 0x101000000000, 1 << 30, 1 << 30, "sheap")
	STACK_DEF(LOCAL_STK, 0x102000000000, 1 << 30, 1 << 30, "local")
	STACK_DEF(TRAIL_STK, 0x103000000000, 1 << 30, 1 << 30, "trail")
#endif
#ifndef _PL_CFG_ARCH_H_
#define _PL_CFG_ARCH_H_
#define HWREG_SP	"%r12"
#define HWREG_FP	"%r13"
#define HWREG_HP	"%r14"
#define HWREG_BTP	"%r15"
#define HWREG_TP	"%rbx"
#define HWREG_ARGS	"%rbp"
#define	PL_ARCH		"x86-64-linux"
#define C_OPTIONS	"-fno-builtin -funsigned-char -O2 -g"
#define ASM_JMP(L)	asm("jmp " ASM_LBL_STR(L))
#endif
