/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "Prolog.h"
#include "pl-main.h"
#include "pl-init.h"


void pl_exit(int status)
{ exit_io();

  exit(status);
}

int main(int argc, char **argv)
{ init_stacks();
  init_ops();
  init_io();
  init_prolog_flag();
  init_os();
  init_time();
  init_argv(argc, argv);

  module__user();

  pl_exit(0);
  return(0);
}

