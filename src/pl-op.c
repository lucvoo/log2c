/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "Prolog.h"
#include "pl-op.h"
#include "pl-init.h"		// For PL_init_ops()


extern atom__t ATOM__fx;
extern atom__t ATOM__fy;
extern atom__t ATOM__xf;
extern atom__t ATOM__yf;
extern atom__t ATOM__xfx;
extern atom__t ATOM__xfy;
extern atom__t ATOM__yfx;
extern atom__t ATOM__yfy;

typedef struct { short 	type;
                 short	prec;
               } op_type;

typedef struct operator__t *operator_t;  
struct operator__t { atom_t	operator;
		     op_type    type[3];
		     operator_t	next;
		   };

#define OP_HASH_SIZE	256
#define OperatorHashValue(Op)	(((Op)->hash) % OP_HASH_SIZE)

static
operator_t operators[OP_HASH_SIZE];


inline static
op_type *get_op_type(atom_t name, int fix)
{ operator_t op;

  for (op=operators[OperatorHashValue(name)]; op; op=op->next)
  { if (op->operator!=name) continue;
    return(&(op->type[fix-1]));
  }

  return(0);
}

inline static
operator_t get_operator(atom_t name)
{ operator_t op;

  for (op=operators[OperatorHashValue(name)]; op; op=op->next)
  { if (op->operator!=name) continue;
    return(op);
  }

  return(0);
}

inline static
int OperatorAtom2Type(atom_t type)
{ if (type==ATOM(_fx))  return(OP_FX);
  if (type==ATOM(_fy))  return(OP_FY);
  if (type==ATOM(_xf))  return(OP_XF);
  if (type==ATOM(_yf))  return(OP_YF);
  if (type==ATOM(_xfx)) return(OP_XFX);
  if (type==ATOM(_xfy)) return(OP_XFY);
  if (type==ATOM(_yfx)) return(OP_YFX);
  if (type==ATOM(_yfy)) return(OP_YFY);

  return(0);
}

inline static
atom_t OperatorType2Atom(int type)
{ switch(type)
  { case OP_FX:  return(ATOM(_fx));
    case OP_FY:  return(ATOM(_fy));
    case OP_XF:  return(ATOM(_xf));
    case OP_YF:  return(ATOM(_yf));
    case OP_XFX: return(ATOM(_xfx));
    case OP_XFY: return(ATOM(_xfy));
    case OP_YFX: return(ATOM(_yfx));
    case OP_YFY: return(ATOM(_yfy));
    default:     return(0);
  }
}


inline static
void add_operator(int precedence, int type, atom_t operator)
{ operator_t op;
  hash_t h;
  op_type    *op_t;

  op=get_operator(operator);

  if (op==0)
    { h=OperatorHashValue(operator);
      op=NEW(*op);
      op->next=operators[h];
      operators[h]=op;
      op->operator=operator;
    }

  op_t=&(op->type[(type>>2)-1]);
  op_t->type=type;
  op_t->prec=precedence;
}


int PL_is_op(int fix, atom_t operator, int *type, int *prec)
{ op_type   *op_t;

  if (!(op_t=get_op_type(operator,fix)) )
    return(0);

  if (op_t->prec <= 0)
    return(0);

  *type=op_t->type;
  *prec=op_t->prec; 

  return(1);
}

int PL_can_be_op(atom_t operator)
{ return(get_operator(operator)!=0); }


int pl_op(term_t precedence, term_t type, term_t operator)
{ atom_t op,   a_t;
  int    prec, t;

  if (!(op = PL_get_atom(operator)) ||
      !(a_t= PL_get_atom(type)) ||
      !PL_get_integer(precedence, &prec) ||
      prec < 0 || prec > 1200 ||
      !(t = OperatorAtom2Type(a_t)) )
    fail;

  add_operator(prec,t,op);

  succeed;
}


int pl_current_op(term_t precedence, term_t type, term_t operator, control_t ctrl)
{ operator_t op;
  int        prec, t, fix;
  atom_t     a_t, a_op; 
  struct { hash_t hash; operator_t op; int fix; } *ctxt;
  hash_t h;
  op_type   *op_t;

  switch(GetCtrl(ctrl))
  { case FIRST_CALL:
	ctxt=AllocCtxt(*ctxt);
	h=OP_HASH_SIZE-1;
	op=operators[h];
	break;
    case NEXT_CALL:
	ctxt=GetCtxt(ctrl);
	h=ctxt->hash;
	op=ctxt->op;
	break;
    default:
	fail;
  }
  
  if (!(a_op = PL_get_atom(operator)))
  { if (isVar(operator))  a_op=0;
    else fail;
  }

  if (!PL_get_integer(precedence, &prec))
  { if (isVar(precedence))  prec=0;
    else fail;
  }

  if ((a_t = PL_get_atom(type)))
  { if (!(t=OperatorAtom2Type(a_t))) fail;
    else ;
  }
  else
  if (isVar(type)) t=0;
  else fail;


  for ( ; h >= 0; op=operators[--h])
  { for ( ; op; op=op->next)
    { if ((a_op && a_op!=op->operator))
        continue;
      else
      { for (fix=0;fix<3;fix++)
        { op_t= &op->type[fix];
          if ( (op_t->prec==0)	||
               (prec && prec!=op_t->prec) ||
               (t && t!=op_t->type)
             ) continue;

          if (!PL_unify_atom(operator,op->operator) || 
              !PL_unify_atom(type,OperatorType2Atom(op_t->type)) ||
              !PL_unify_intg(precedence, op_t->prec)
             ) fail;

          ctxt->hash=h;
          ctxt->op=op->next;
          ctxt->fix=fix;
          retry;
        } 
      } 
    }
 }

  fail;
}

void PL_init_ops(void)
{ add_operator(200,OP_XFX,ATOM(doublestar));
  add_operator(200,OP_XFY,ATOM(hat));
  add_operator(400,OP_YFX,ATOM(_mod));
  add_operator(400,OP_YFX,ATOM(_rem));
  add_operator(400,OP_YFX,ATOM(_xor));
  add_operator(400,OP_YFX,ATOM(div));
  add_operator(400,OP_YFX,ATOM(divide));
  add_operator(400,OP_YFX,ATOM(lshift));
  add_operator(400,OP_YFX,ATOM(rshift));
  add_operator(400,OP_YFX,ATOM(star));
  add_operator(500,OP_FX,ATOM(backslash));
  add_operator(500,OP_FX,ATOM(minus));
  add_operator(500,OP_FX,ATOM(plus));
  add_operator(500,OP_YFX,ATOM(and));
  add_operator(500,OP_YFX,ATOM(minus));
  add_operator(500,OP_YFX,ATOM(or));
  add_operator(500,OP_YFX,ATOM(plus));
  add_operator(600,OP_XFY,ATOM(module));
  add_operator(700,OP_XFX,ATOM(_is));
  add_operator(700,OP_XFX,ATOM(ar_equals));
  add_operator(700,OP_XFX,ATOM(ar_larger));
  add_operator(700,OP_XFX,ATOM(ar_larger_equal));
  add_operator(700,OP_XFX,ATOM(ar_not_equal));
  add_operator(700,OP_XFX,ATOM(ar_smaller));
  add_operator(700,OP_XFX,ATOM(ar_smaller_equal));
  add_operator(700,OP_XFX,ATOM(at_equals));
  add_operator(700,OP_XFX,ATOM(at_larger));
  add_operator(700,OP_XFX,ATOM(at_larger_equal));
  add_operator(700,OP_XFX,ATOM(at_not_equals));
  add_operator(700,OP_XFX,ATOM(at_smaller));
  add_operator(700,OP_XFX,ATOM(at_smaller_equal));
  add_operator(700,OP_XFX,ATOM(not_strick_equals));
  add_operator(700,OP_XFX,ATOM(not_unifiable));
  add_operator(700,OP_XFX,ATOM(strick_equals));
  add_operator(700,OP_XFX,ATOM(unify));
  add_operator(700,OP_XFX,ATOM(univ));
//  add_operator(900,OP_FY,ATOM(_not));
  add_operator(900,OP_FY,ATOM(not_provable));
  add_operator(900,OP_FY,ATOM(__2B_3E));
  add_operator(1000,OP_XFY,ATOM(comma));
  add_operator(1050,OP_XFY,ATOM(ifthen));
  add_operator(1050,OP_XFY,ATOM(softcut));
  add_operator(1100,OP_XFY,ATOM(bar));
  add_operator(1100,OP_XFY,ATOM(semicolon));
  add_operator(1200,OP_FX,ATOM(prove));
  add_operator(1200,OP_XFX,ATOM(grammar));
  add_operator(1200,OP_XFX,ATOM(prove));
  add_operator(1200,OP_XFX,ATOM(__3A_2B));
  add_operator(1150,OP_FX,ATOM(_module__transparent));
}
