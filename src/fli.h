/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef	_FLI_H_
#define _FLI_H_

#include "pl.h"
#include "pl-inline.h"
#include <stdio.h>
#include <string.h>

#define succeed	return(SUCCEED)
#define fail	return(FAIL)
#define retry	return(RETRY)
#define try(G)	if (!(G)) fail; else

#define Tag(v)	(v & TAG_MASK)
#define Val(c)	({ union cell d=*c; while (Tag(d.val)==MK_TAG(ref_tag)) { d=*(d.celp); }; d.val; })

#define	__isVar(v)	(    v ==MK_TAG(var_tag))
#define __isAtom(v) 	(Tag(v)==MK_TAG(ato_tag))
#define __isInteger(v) 	(Tag(v)==MK_TAG(int_tag))
#define __isFloat(v) 	(Tag(v)==MK_TAG(flt_tag))
#define __isTerm(v) 	(Tag(v)==MK_TAG(fun_tag))
#define	__isCons(v)	(v==__fun(FUN(dot,2)))
#define	__isNil(v)	(v==(MK_TAG(ato_tag) + (unsigned long) ATOM(nil)))

#define __isNumber(V)	({ unsigned long v=V; int r=(__isInteger(v) || __isFloat(v)); r; })
#define	__isStruct(V)	({ unsigned long v=V; int r=(v!=__fun(FUN(dot,2)) && __isTerm(v)); r; })
#define __isAtomic(V)	({ unsigned long v=V; int r=(!__isVar(v) && !__isTerm(v)); r; })
#define __isList(V)	({ unsigned long v=V; int r=(__isCons(v) || __isNil(v)); r; })

#define	Mark(m)	do { m.trail =TP; m.global=HP; } while (0)
#define	Undo(m)	do { reset(m.trail); HP=m.global; } while(0)

int pl_unify(union cell *, union cell *);

inline static int PL_try_unify(union cell *a, union cell *b)
{
	struct mark m;
	int rval;

	Mark(m);
	if (!(rval = pl_unify(a, b)))
		Undo(m);

	return rval;
}

inline static void *AllocLocal(size_t n)
{
	void *ptr = (SP + 1);		// SP is postincrement.
	SP += Adjust(n);
	return ptr;
}

inline static void *AllocGlobal(size_t n)
{
	void *ptr = HP;
	HP += Adjust(n);
	return ptr;
}

inline static void *AllocHeap(size_t n)
{
	void *ptr = SHP;
	SHP += Adjust(n);
	return ptr;
}

inline static void *PL_foreign_context(enum control *c)
{
	return ((void *)c) + CELL_SIZE;
}

inline static enum control PL_foreign_control(enum control *c)
{
	return *c;
}

#define	GetCtxt(C)	PL_foreign_context(C)
#define	GetCtrl(C)	PL_foreign_control(C)
#define	PL_retry(C)	return(RETRY)

#define NEW(E)		((typeof(E) *) AllocHeap(sizeof(E)) )
#define NEW_(E,N)	((typeof(E) *) AllocHeap(sizeof(E)*(N)) )

#define AllocCtxt(T)	AllocLocal(sizeof(T))

#define PL_warning(fm,args...)	do { fflush(0); fprintf(stderr,"[Warning: " fm "]\n" , ## args); return(0); } while(0)
#define PL_warn(fm,args...)	do { fflush(0); fprintf(stderr,"[Warning: " fm "]\n" , ## args); } while(0)

#define PL_syntax_error(fm) \
	do { fflush(0); \
	     if (Sread->pos) \
	       fprintf(stderr,"[Syntax error (%ld:%ld): " fm "]\n" , \
	       		Sread->pos->line_no, Sread->pos->col_no); \
	     else \
	       fprintf(stderr,"[Syntax error : " fm "]\n"); \
	     return(0); \
	} while(0)

/**********************************************************************/

#endif
