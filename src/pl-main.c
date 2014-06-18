/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-main.h"
#include "pl-init.h"

void pl_exit(int status)
{
	PL_exit_io();

	exit(status);
}

int main(int argc, char **argv)
{
	PL_init_stacks();
	PL_init_ops();
	PL_init_io();
	PL_init_prolog_flag();
	PL_init_os();
	PL_init_time();
	PL_init_argv(argc, argv);

	module__user();

	pl_exit(0);
	return 0;
}
