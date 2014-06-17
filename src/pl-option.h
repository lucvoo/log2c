/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef PL_OPTIONS_H_
#define PL_OPTIONS_H_

enum pl_option_type { OPT_BOOL, OPT_INTG, OPT_ATOM, OPT_TERM };

union pl_option_val {
	long *intg;
	int *bool;
	const char **str;
	struct atom **atom;
	union cell *cell;
	union cell **term;
};

struct pl_option_spec {
	const struct atom *name;
	const enum pl_option_type type;
	union pl_option_val val;
};

extern
int PL_scan_options(union cell *options, struct pl_option_spec *spec);

#endif
