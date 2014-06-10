/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	_FLI_H_
#define _FLI_H_

#include "pl.h"
#include "pl-inline.h"
#include <stdio.h>
#include <string.h>


#define PL_REFERENCE	(ref_tag)
#define PL_VARIABLE	(var_tag)
#define PL_ATOM		(ato_tag)
#define PL_INTEGER	(int_tag<<TAG_POS)
#define PL_FLOAT	(flt_tag<<TAG_POS)
#define PL_TERM		(fun_tag<<TAG_POS)


#define succeed	return(SUCCEED)
#define fail	return(FAIL)
#define retry	return(RETRY)
#define try(G)	if (!(G)) fail; else 


#define Tag(v)	(v & TAG_MASK)
#define Val(c)	({ cell_t d=*c; while (Tag(d.val)==(ref_tag<<TAG_POS)) { d=*(d.celp); }; d.val; })

#define	__isVar(v)	(v==(var_tag<<TAG_POS))
#define __isAtom(v) 	(Tag(v)==(ato_tag<<TAG_POS))
#define __isInteger(v) 	(Tag(v)==(int_tag<<TAG_POS))
#define __isFloat(v) 	(Tag(v)==(flt_tag<<TAG_POS))
#define __isTerm(v) 	(Tag(v)==(fun_tag<<TAG_POS))
#define	__isCons(v)	(v==__fun(FUN(dot,2)))
#define	__isNil(v)	(v==((ato_tag<<TAG_POS) + (unsigned int) ATOM(nil)))

#define __isNumber(V)	({ unsigned int v=V; int r=(__isInteger(v) || __isFloat(v)); r; })
#define	__isStruct(V)	({ unsigned int v=V; int r=(v!=__fun(FUN(dot,2)) && __isTerm(v)); r; })
#define __isAtomic(V)	({ unsigned int v=V; int r=(!__isVar(v) && !__isTerm(v)); r; })
#define __isList(V)	({ unsigned int v=V; int r=(__isCons(v) || __isNil(v)); r; })




#define	Mark(m)	do { m.trail =TP; m.global=HP; } while (0)
#define	Undo(m)	do { reset(m.trail); HP=m.global; } while(0)

int pl_unify(cell_t *, cell_t *);

INLINE_DECL
int PL_try_unify(cell_t *a, cell_t *b)
{ mark_t m;
  int rval;

  Mark(m);
  if (!(rval=pl_unify(a,b)))
    Undo(m);

  return(rval);
}


INLINE_DECL
void *AllocLocal(size_t n)
{ void *ptr=(SP+1);	// SP is postincrement.
  SP+=Adjust(n);
  return(ptr);
}

INLINE_DECL
void *AllocGlobal(size_t n)
{ void *ptr=HP;
  HP+=Adjust(n);
  return(ptr);
}

INLINE_DECL
void *AllocHeap(size_t n)
{ void *ptr=SHP;
  SHP+=Adjust(n);
  return(ptr);
}

INLINE_DECL
void *PL_foreign_context(control_t c)
{ return(((void *) c) + CELL_SIZE); }

INLINE_DECL
ctrl_t PL_foreign_control(control_t c)
{ return(*c); }

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

#endif	// _FLI_H_
