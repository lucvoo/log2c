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

#ifndef  PL_STACK_H_
#define  PL_STACK_H_

struct pl_stack_stat  {
  struct { int heap;
	   int local;
	   int trail;
	   int sheap;
	 } used, free; };

int PL_get_stack_stat(struct pl_stack_stat *);

#endif	// PL_STACK_H_
