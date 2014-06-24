/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef PL_EXT_H_
#define PL_EXT_H_

#ifndef PL

#define PL_PROTO_D(A)	PL_PROTO_D_##A
#define PL_PROTO_N(A)	PL_PROTO_N_##A

#define PL_PROTO_1	union cell *
#define PL_PROTO_2	PL_PROTO_1, union cell *
#define PL_PROTO_3	PL_PROTO_2, union cell *
#define PL_PROTO_4	PL_PROTO_3, union cell *
#define PL_PROTO_5	PL_PROTO_4, union cell *
#define PL_PROTO_6	PL_PROTO_5, union cell *
#define PL_PROTO_7	PL_PROTO_6, union cell *
#define PL_PROTO_8	PL_PROTO_7, union cell *

#define PL_PROTO_D_0	void
#define PL_PROTO_D_1	PL_PROTO_1
#define PL_PROTO_D_2	PL_PROTO_2
#define PL_PROTO_D_3	PL_PROTO_3
#define PL_PROTO_D_4	PL_PROTO_4
#define PL_PROTO_D_5	PL_PROTO_5
#define PL_PROTO_D_6	PL_PROTO_6
#define PL_PROTO_D_7	PL_PROTO_7
#define PL_PROTO_D_8	PL_PROTO_8

#define PL_PROTO_N_0
#define PL_PROTO_N_1	PL_PROTO_1,
#define PL_PROTO_N_2	PL_PROTO_2,
#define PL_PROTO_N_3	PL_PROTO_3,
#define PL_PROTO_N_4	PL_PROTO_4,
#define PL_PROTO_N_5	PL_PROTO_5,

#define DET(P,A,F)	int F(PL_PROTO_D(A));
#define NDET(P,A,F)	int F(PL_PROTO_N(A) enum control *);

#else
#define DET(F,N,C)      foreign_pred_builtin(F,N,C,det).
#define NDET(F,N,C)     foreign_pred_builtin(F,N,C,ndet).
#endif

#include "pl-ext.def"

#undef	DET
#undef	NDET

#endif
