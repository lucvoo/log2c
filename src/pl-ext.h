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

#define PL_PROTO_D_0	void);
#define PL_PROTO_D_1	term_t);
#define PL_PROTO_D_2	term_t, term_t);
#define PL_PROTO_D_3	term_t, term_t, term_t);
#define PL_PROTO_D_4	term_t, term_t, term_t, term_t);
#define PL_PROTO_D_5	term_t, term_t, term_t, term_t, term_t);
#define PL_PROTO_D_6	term_t, term_t, term_t, term_t, term_t, term_t);
#define PL_PROTO_D_7	term_t, term_t, term_t, term_t, term_t, term_t, term_t);
#define PL_PROTO_D_8	term_t, term_t, term_t, term_t, term_t, term_t, term_t, term_t);

#define PL_PROTO_N_0	enum control *);
#define PL_PROTO_N_1	term_t, enum control *);
#define PL_PROTO_N_2	term_t, term_t, enum control *);
#define PL_PROTO_N_3	term_t, term_t, term_t, enum control *);
#define PL_PROTO_N_4	term_t, term_t, term_t, term_t, enum control *);
#define PL_PROTO_N_5	term_t, term_t, term_t, term_t, term_t, enum control *);

#define DET(P,A,F)	int F(PL_PROTO_D(A)
#define NDET(P,A,F)	int F(PL_PROTO_N(A)

#else
#define DET(F,N,C)      foreign_pred_builtin(F,N,C,det).
#define NDET(F,N,C)     foreign_pred_builtin(F,N,C,ndet).
#endif

#include "pl-ext.def"

#undef	DET
#undef	NDET

#endif
