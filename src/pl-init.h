/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_INIT_H_
#define	PL_INIT_H_

extern void init_argv(int argc, char **argv);
extern void init_io(void);
extern void exit_io(void);
extern void init_ops(void);
extern void init_os(void);
extern void init_prolog_flag(void);
extern void init_stacks(void);
extern void init_time(void);

#endif
