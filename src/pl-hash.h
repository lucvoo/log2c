/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef PL_HASH_H_
#define PL_HASH_H_

#include "pl-types.h"


hash_t hpjw(const char* x); // From Dragon book, p436
hash_t hpjw_2(const char *x, const char *y);
hash_t mult_hash(int x);

#endif	// PL_HASH_H_
