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

typedef enum { ref_tag, var_tag, ato_tag,
	fun_tag, int_tag, flt_tag
} tag_t;

typedef enum { FAIL, SUCCEED, RETRY } foreign_t;
typedef enum { FIRST_CALL, NEXT_CALL } ctrl_t, *control_t;

#ifdef	WORDS_BIGENDIAN

typedef struct {
	tag_t tag:TAG_BITS;
	long val:TAG_POS;
} _val_t;
typedef struct {
	tag_t tag:TAG_BITS;
	unsigned long uval:TAG_POS;
} uval_t;

#else					// WORDS_BIGENDIAN

typedef struct {
	long val:TAG_POS;
	tag_t tag:TAG_BITS;
} _val_t;
typedef struct {
	unsigned long uval:TAG_POS;
	tag_t tag:TAG_BITS;
} uval_t;

#endif					// WORDS_BIGENDIAN

typedef union cell_t cell_t, *term_t, *tr_t;
union cell_t {
	unsigned long val;
	_val_t tag_val;
	uval_t tag_uval;
	cell_t *celp;
};

struct atom {
	cell_t atom;
	const char *name;
	hash_t hash;
	struct atom *next;
};

struct functor {
	struct atom *functor;
	long arity;
	struct functor *next;
};

typedef union pl_stack_t pl_stack_t;
union pl_stack_t {
	cell_t *celp;
	tr_t *tr;
	pl_stack_t *stk;
	void *cod;
	long intg;
	cell_t cell;
	ctrl_t ctrl;
};

typedef struct {
	tr_t *trail;
	cell_t *global;
} mark_t;

typedef void *predicate_t, *pred_t;

typedef struct jmp__t jmp__t;
struct jmp__t {
	struct atom *functor;
	long arity;
	void *pred;
	jmp__t *next;
};
typedef struct {
	jmp__t **tab;
	int size;
} jmp_table;

typedef struct {
	const char *file;
	struct atom *module;
	jmp_table pub;
	jmp_table all;
} module_t;

typedef struct mods_t modules_t;
struct mods_t {
	struct atom *name;
	module_t *module;
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

#ifndef	STRUCT_PL_STREAM
#define STRUCT_PL_STREAM
typedef struct pl_stream_t pl_stream_t, *pl_stream;
#endif

// extern pl_stream Stdin, Stdout, Stderr;
// extern pl_stream_t Stdin__, Stdout__, Stderr__;

#endif
