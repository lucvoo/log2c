/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

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
