/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef	PL_STATUS_H_
#define PL_STATUS_H_

#include "Prolog.h"

typedef struct { atom_t bck_quotes;
		 int	char_esc;
		 int    char_conv;
                 atom_t dbl_quotes;
                 int	debug;
                 int	discont;
                 int	dollar;
                 int	file_err;
                 atom_t float_fmt;
                 int	iso;
                 int	long_atom;
                 int	nested_com;
                 int	rep_err;
                 int	singleton;
                 int	string;
                 int	tty_ctrl;
               } pl_status_t;

extern pl_status_t PL__status;


#endif	// PL_STATUS_H_
