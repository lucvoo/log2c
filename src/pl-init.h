/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_INIT_H_
#define	PL_INIT_H_

extern void PL_init_argv(int argc, char **argv);
extern void PL_init_io(void);
extern void PL_exit_io(void);
extern void PL_init_ops(void);
extern void PL_init_os(void);
extern void PL_init_prolog_flag(void);
extern void PL_init_stacks(void);
extern void PL_init_time(void);

#endif
