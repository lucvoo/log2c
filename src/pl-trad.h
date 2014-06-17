/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef	PL_TRAD_H_
#define PL_TRAD_H_

#include "Prolog.h"
#include <stdlib.h>			// for exit( )

void pl_exit(int);

#define	PL_ARG(n)	PL_ARGS[n]

#define	FAILED			\
	printf("No\n");		\
	pl_exit(1)

/*
#define getref_m(ref)			\
	({ int n;			\
	   if (!PL_eval_(ref,&n))	\
	     goto backtrack;		\
	   return(n);			\
	})
*/

#define getatom(arg,A)			\
	{ if (isatom(A,arg)) ;		\
	  else				\
	  if (is_var(arg))		\
	    { PL_put_atom(arg,A);	\
	      trail(arg);		\
	    }				\
	  else				\
	    goto backtrack;		\
	}

#define GETINTG(C,I)			\
	{ int __i;			\
	  if (PL_eval_((C),&__i))	\
	    I=__i;			\
	  else				\
	    goto backtrack;		\
	}

#define getintg(arg,N)			\
	{ if (isintg(N,arg)) ;		\
	  else				\
	  if (is_var(arg))		\
	    { PL_put_integer(arg,N);	\
	      trail(arg);		\
	    }				\
	  else 				\
	    goto backtrack;		\
	}

#define getflt(arg,N)			\
	{ if (isflt(N,arg)) ;		\
	  else				\
	  if (is_var(arg))		\
	    { PL_put_float(arg,N);	\
	      trail(arg);		\
	    }				\
	  else 				\
	    goto backtrack;		\
	}

#define getref(arg,ref)			\
	{ if ( !unify(arg,ref) )	\
	    goto backtrack;		\
	}

#define uatom_r(A,V)			\
	{ cell_t *v=deref(V);		\
					\
	  if (isatom(A,v)) ;		\
	  else				\
	  if (is_var(v))		\
	  { PL_put_atom(v,A);		\
	    trail(v);			\
	  }				\
	  else				\
	    goto backtrack;		\
	}

inline static void setbtp(void *L)
{
	FP[1].stk = BTP;
	FP[2].tr = TP;
	FP[3].celp = HP;
	FP[4].cod = L;
	BTP = FP;
}

inline static void nextalt(void *L)
{
	FP[4].cod = L;
}

inline static void delbtp(void)
{
	BTP = FP[1].stk;
}

#define popenv()			\
	{ void *PC;			\
					\
	  if (FP > BTP)			\
	    SP=FP-2;			\
					\
	  PC=FP[-1].cod;		\
	  FP=FP[0].stk;			\
	  goto *PC;			\
	}

#define restore()			\
	{ void *PC;			\
	  PC=FP[-1].cod;		\
	  FP=FP[0].stk;			\
	  goto *PC;			\
	}

inline static void init(void *L)
{
	FP = STK + 1;
	BTP = FP;
	TP = TR_STK;
	HP = H_STK;
	FP[2].tr = (typeof(TP)) - 1;
	FP[3].celp = (typeof(HP)) - 1;
	FP[4].cod = L;
#ifdef HWREG_ARGS
	PL_ARGS = &PL_ARGS_[0];
#endif
}

#define halt_()				\
	{ if (PL_next_goal())		\
	    goto backtrack;		\
	  else				\
	  { pl_exit(0);			\
	  }				\
	}

inline static void cut_deep(void)
{
	while (BTP >= FP)		// FIXME : SEGV in : main :- p -> ! ; true.
		BTP = BTP[1].stk;
}

inline static void cut(void)
{
	while (BTP > FP)
		BTP = BTP[1].stk;
}

inline static void not_0(void *L)
{
	SP += 2;			// enter();
	SP[0].stk = FP;
	SP[1].stk = BTP;
	SP[2].tr = TP;
	SP[3].celp = HP;
	SP[4].cod = L;
	BTP = SP;
	SP += 4;
}

inline static void alt_0(void *L)
{
	SP[2].stk = FP;
	SP[3].stk = BTP;
	SP[4].tr = TP;
	SP[5].celp = HP;
	SP[6].cod = L;
	BTP = SP + 2;
	SP += 6;
}

inline static void alt_1(void *L)
{
	SP = FP + 4;
	FP[4].cod = L;
	FP = FP->stk;
}

inline static void alt_2(void)
{
	BTP = FP[1].stk;		// delbtp();
	if (FP > BTP)
		SP = FP - 2;
	else
		SP = FP;
	FP = FP->stk;
}

inline static void unify_var(cell_t * a1, cell_t * a2)
{
	mkrefp(a1, a2);
	trail(a1);
}

/* Prolog Arithmetic operators */
#define PL_OP__2B_2(A,B)	(A + B)
#define PL_OP__2D_2(A,B)	(A - B)
#define PL_OP__2D_1(A)		(- A)
#define PL_OP__2A_2(A,B)	(A * B)
#define PL_OP__2F_2F_2(A,B)	(A / B)
#define PL_OP__2F_2(A,B)	(A / B)
#define PL_OP_mod_2(A,B)	(A % B)
#define PL_OP_rem_2(A,B)	(A % B)

#define PL_OP__2F_5C_2(A,B)	(A & B)
#define PL_OP__5C_2F_2(A,B)	(A | B)
#define PL_OP_xor_2(A,B)	(A ^ B)
#define PL_OP__5C_1(A)		( ~ A)

#define PL_OP__3C_3C_2(A,B)	(A << B)
#define PL_OP__3E_3E_2(A,B)	(A >> B)

#define PL_OP__2A_2A_2(A,B)	({ int r;			\
				   if (B<0) goto backtrack;	\
				   for (r=1;B;B--)		\
				     r*=A;			\
				   r;				\
				})
#define PL_OP__5E_2(A,B)	PL_OP__2A_2A_2(A,B)

#define PL_OP_abs_1(A)		((A<0) ? -A : A)
#define PL_OP_sign_1(A)		((A<0) ? -1 : ((A>0) ? 1 : 0))

#define PL_OP_max_2(A,B)	((A>B) ? A : B)
#define PL_OP_min_2(A,B)	((A<B) ? A : B)

#define expand_macro(M)	M

#define stringifx(S)	#S
#define stringify(S)	stringifx(S)

#define ASM_LBL_STR(L)	stringify(__USER_LABEL_PREFIX__) stringify(L)

#define VM_CALL(P,L)	SP[1].cod= &&L; SP[2].stk=FP; FP=SP+2; P

#endif
