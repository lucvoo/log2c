/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef PL_OPTIONS_H_
#define PL_OPTIONS_H_

enum pl_option_type {
	OPT_BOOL,
	OPT_INTG,
	OPT_ATOM,
	OPT_ATOMS,
	OPT_TERM,
};

union pl_option_val {
	long *intg;
	int *bool;
	struct atom **atom;
	union cell **term;
	unsigned long *flags;
};

struct pl_option_map {
	const struct atom	*name;
	int			val;
};

struct pl_option_spec {
	const struct atom *name;
	const enum pl_option_type type;
	union pl_option_val val;
	union {
	const struct pl_option_map *map;	// optional null terminated array of ...
	};
	unsigned int shift;
};

extern
int PL_scan_options(union cell *options, struct pl_option_spec *spec);

#endif
