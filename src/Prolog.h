/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef INLINE_DECL
#ifdef  PL_DEBUG
#define INLINE_DECL inline extern
#else
#define INLINE_DECL inline static
#endif
#endif

#include "pl.h"
#include "pl-inline.h"
#include "fli.h"
#include "pl-fli.h"
#include "pl-pred.h"
#include "pl-prims.h"
#include "pl-ext.h"
