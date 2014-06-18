/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef PL_TYPES_H_
#define PL_TYPES_H_

#include "pl-config.h"
#include <sys/types.h>

typedef unsigned long hash_t;

enum tag {
	ref_tag,
	var_tag,
	ato_tag,
	fun_tag,
	int_tag,
	flt_tag
};

enum foreign { FAIL, SUCCEED, RETRY };
enum control { FIRST_CALL, NEXT_CALL };

#ifdef	WORDS_BIGENDIAN

struct tag_sval {
	enum tag tag:TAG_BITS;
	long val:TAG_POS;
};
struct tag_uval {
	enum tag tag:TAG_BITS;
	unsigned long uval:TAG_POS;
};

#else					// WORDS_BIGENDIAN

struct tag_sval {
	long val:TAG_POS;
	enum tag tag:TAG_BITS;
};
struct tag_uval {
	unsigned long uval:TAG_POS;
	enum tag tag:TAG_BITS;
};

#endif					// WORDS_BIGENDIAN

union cell {
	unsigned long val;
	struct tag_sval tag_sval;
	struct tag_uval tag_uval;
	union cell *celp;
};

struct atom {
	union cell atom;
	const char *name;
	hash_t hash;
	struct atom *next;
};

struct functor {
	struct atom *functor;
	long arity;
	struct functor *next;
};

union pl_stack {
	union cell *celp;
	union cell **tr;
	union pl_stack*stk;
	void *cod;
	long intg;
	union cell cell;
	enum control ctrl;
};

struct mark {
	union cell **trail;
	union cell *global;
};

struct jmp {
	struct atom *functor;
	long arity;
	void *pred;
	struct jmp *next;
};

struct jmp_table {
	struct jmp **tab;
	int size;
};

struct module {
	const char *file;
	struct atom *module;
	struct jmp_table pub;
	struct jmp_table all;
};

struct modules {
	struct atom *name;
	struct module *module;
	struct modules *next;
};


#define	FALSE	0
#define	TRUE	1

#endif
