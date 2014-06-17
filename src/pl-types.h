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
typedef long intg_t;
typedef double flt_t;

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

typedef struct {
	enum tag tag:TAG_BITS;
	long val:TAG_POS;
} _val_t;
typedef struct {
	enum tag tag:TAG_BITS;
	unsigned long uval:TAG_POS;
} uval_t;

#else					// WORDS_BIGENDIAN

typedef struct {
	long val:TAG_POS;
	enum tag tag:TAG_BITS;
} _val_t;
typedef struct {
	unsigned long uval:TAG_POS;
	enum tag tag:TAG_BITS;
} uval_t;

#endif					// WORDS_BIGENDIAN

union cell {
	unsigned long val;
	_val_t tag_val;
	uval_t tag_uval;
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

typedef void *predicate_t, *pred_t;

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

typedef struct mods_t modules_t;
struct mods_t {
	struct atom *name;
	struct module *module;
	modules_t *next;
};

typedef struct {
	int type;			// int_tag | flt_tag
	union {
		intg_t intg;
		flt_t flt;
		unsigned long w[sizeof(double) / sizeof(unsigned long)];
	} val;
} pl_number_t;

typedef int bool;
#define	FALSE	0
#define	TRUE	1
#define	EOS	'\0'

#include <sys/time.h>

#ifndef TIME_OF_DAY
typedef struct {
	double utime;
	double stime;
} time__t;
#endif

#endif
