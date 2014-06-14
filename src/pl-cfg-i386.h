/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifdef    STACK_DEF
//         symbol  , address   , incr , max  , name
STACK_DEF( HEAP_STK, 0x10000000, 1<<18, 1<<23, "heap" )
STACK_DEF(SHEAP_STK, 0x18000000, 1<<15, 1<<22, "sheap")
STACK_DEF(LOCAL_STK, 0x20000000, 1<<15, 1<<22, "local")
STACK_DEF(TRAIL_STK, 0x28000000, 1<<15, 1<<22, "trail")
#endif


#ifndef _PL_CFG_ARCH_H_
#define _PL_CFG_ARCH_H_

#define HWREG_SP	"%ebx"
#define HWREG_FP	"%esi"
#define HWREG_HP	"%edi"
//#define HWREG_BTP	"%ebx"
//#define HWREG_TP	"%ecx"

#define	PL_ARCH		"i486-linux"
#define C_OPTIONS	"-fno-builtin -funsigned-char -O2 -ggdb -ffixed-%ebx -ffixed-%esi -ffixed-%edi"

#endif
