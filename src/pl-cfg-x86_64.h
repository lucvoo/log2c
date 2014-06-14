/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifdef    STACK_DEF
//         symbol  , address   , incr , max  , name
STACK_DEF( HEAP_STK, 0x100000000000, 1<<28, 1<<30, "heap" )
STACK_DEF(SHEAP_STK, 0x101000000000, 1<<28, 1<<30, "sheap")
STACK_DEF(LOCAL_STK, 0x102000000000, 1<<28, 1<<30, "local")
STACK_DEF(TRAIL_STK, 0x103000000000, 1<<28, 1<<30, "trail")
#endif


#ifndef _PL_CFG_ARCH_H_
#define _PL_CFG_ARCH_H_

//#define HWREG_SP	"%"
//#define HWREG_FP	"%"
//#define HWREG_HP	"%"
////#define HWREG_BTP	"%"
////#define HWREG_TP	"%"

#define	PL_ARCH		"x86-64-linux"
#define C_OPTIONS	"-fno-builtin -funsigned-char -O2 -g"

#endif
