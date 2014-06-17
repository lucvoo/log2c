/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef	PL_STATUS_H_
#define PL_STATUS_H_

#include "Prolog.h"

struct pl_status {
	struct atom *bck_quotes;
	int char_esc;
	int char_conv;
	struct atom *dbl_quotes;
	int debug;
	int discont;
	int dollar;
	int file_err;
	struct atom *float_fmt;
	int iso;
	int long_atom;
	int nested_com;
	int rep_err;
	int singleton;
	int string;
	int tty_ctrl;
};

extern struct pl_status PL__status;

#endif
