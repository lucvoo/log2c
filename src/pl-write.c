/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/


#include "Prolog.h"
#include "pl-stream.h"
#include "pl-ctype.h"
#include "pl-op.h"
#include "pl-fli.h"
#include "pl-status.h"
#include "pl-io.h"
#include "pl-write.h"
#include <stdio.h>
#include <math.h>		// For isnan()/isinf()

#include "pl-string.h"

#define	SHORT_BUF_SIZE	33	// FIXME : magic number

#define	OFlags(n)	(1<<(n))

typedef enum 
{ OPT_QUOT	= OFlags(0),
  OPT_ESC	= OFlags(1),
  OPT_OPS	= OFlags(2),
  OPT_NUMV	= OFlags(3),
  OPT_NAMV	= OFlags(4),
  OPT_SPAC	= OFlags(5),
  OPT_LIST	= OFlags(6),
  OPT_CURL	= OFlags(7),
  OPT_LAST
} w_flags_t;

#define Options(O)	(opt->flags & (O))

typedef struct
{ long		max_depth;
  term_t	bind;
  w_flags_t	flags;
} w_opt;

// PRE : t is deref
inline static
char *varName(term_t t)
{ long n;
  char s;
  static char buf[SHORT_BUF_SIZE];

  if (t >= (term_t) STK)
  { n=(t - (term_t) STK);
    s='L';
  }
  else
  { n=(t - H_STK);
    s='G';
  }

  sprintf(buf,"_%c%ld",s,n);
  return(buf);
}

//#####################################################################

#define AT_LOWER	0
#define AT_QUOTE	1
#define AT_FULLSTOP	2
#define AT_SYMBOL	3
#define AT_SOLO		4
#define AT_SPECIAL	5


static inline
int atomType(atom_t a)
{ const char *s = PL_atom_chars(a);

  if (isLower(*s))
  { const char *s2;

    for(s2 = s; *s2 && isAlphaNum_(*s2); )
      s2++;
    return(*s2 == '\0' ? AT_LOWER : AT_QUOTE);
  }

  if (a==ATOM(dot))
    return AT_FULLSTOP;
  
  if (isSymbol(*s))
  { const char *s2;

    for(s2 = s; *s2 && isSymbol(*s2); )
      s2++;
    return(*s2 == '\0' ? AT_SYMBOL : AT_QUOTE);
  }

					/* % should be quoted! */
  if ((isSolo(*s) || *s == ',') && s[1] == '\0' && s[0] != '%' )
    return AT_SOLO;

  if ( a == ATOM(nil) || a == ATOM(curl) )
    return AT_SPECIAL;
  
  return AT_QUOTE;
}

//#####################################################################

static int lastc;
static
int Puts(pl_stream S, const char *str)
{ const char *s = str;

  while(*s)
    Sputc(S,*s++);

  if (s != str)
    lastc = s[-1];

  return( ! Serror(S));
}

inline static
int Putc(pl_stream S, int c)
{ 
  lastc = c;

  return Sputc(S, c);
}


inline static
bool PutOpenToken(pl_stream S, int c)
{ 
  if ( lastc != -1 &&
       ( (isAlphaNum_(lastc) && isAlphaNum_(c)) ||
         (isSymbol(lastc) && isSymbol(c)) ||
         c == '('		/* ) */
       )
     )
  { return Putc(S,' ');
  }

  succeed;
}


inline static
bool PutOpenBrace(pl_stream S)
{ return PutOpenToken(S, '(') && Putc(S,'(');
}

inline static
bool PutToken(pl_stream S, const char *s)
{ if (s[0])
    return PutOpenToken(S, s[0]) && Puts(S,s);
  else
    succeed;
}

//#####################################################################
  
static
int WriteQuoted(pl_stream S, const char *s, int quote, const w_opt *opt)
{ char c;

  Putc(S,quote);
  while( (c = *s++) != '\0' )
  { if (Options(OPT_ESC))
    { if ( c >= ' ' && c != 127 && c != quote && c != '\\' )
        Putc(S, c);
      else
      { char esc[5];
        esc[0] = '\\';
        esc[2] = '\0';
        if (c == quote)
          esc[1] = c;
        else
        { switch(c)
          { case '\a': esc[1]='a'; break;
            case '\b': esc[1]='b'; break;
            case '\f': esc[1]='f'; break;
            case '\n': esc[1]='n'; break;
            case '\r': esc[1]='r'; break;
            case '\t': esc[1]='t'; break;
            case '\v': esc[1]='v'; break;
            case '\'': esc[1]='\''; break;
            case '\\': esc[1]='\\'; break;
            default  : sprintf(esc+1, "%03o", c); break;
          } 
        }
        Puts(S, esc);
      }
    }
    else
    { if (c == quote)
        Putc(S,c);
      Putc(S,c);
    }
  }
  Putc(S, quote);

  succeed;
}

static int
WriteAtom(pl_stream S, atom_t a, const w_opt *opt)
{ if (Options(OPT_QUOT))
  { switch( atomType(a) )
    { case AT_LOWER:
      case AT_SYMBOL:
      case AT_SOLO:
      case AT_SPECIAL:
		PutToken(S,PL_atom_chars(a));
		break;
      case AT_QUOTE:
      case AT_FULLSTOP:
      default: WriteQuoted(S, PL_atom_chars(a), '\'', opt);
    }
  }
  else
    PutToken(S,PL_atom_chars(a));

  succeed;
}

inline static
void WritePrimitive(pl_stream S, term_t t, const w_opt *opt)
{ char buf[33];

  t=deref(t);

  switch(get_tag(t))
  { case var_tag:
	PutToken(S,varName(t));
	break;
    case ato_tag:
	WriteAtom(S, get_atom(t), opt);
	break;
    case int_tag:		// Beware of automatic conversion
	{ long i=(long) get_val(t);
	  sprintf(buf,"%ld",i);
	  PutToken(S, buf);
	  break;
	}
    case flt_tag:
	{ double d = get_flt(t);
	  const char *s;
 
	  if (isinf(d))
	  { s = Options(OPT_QUOT) ? "'$Infinity'" : "Infinity";
	  }
	  else
	  if (isnan(d))
	  { s = Options(OPT_QUOT) ? "'$NaN'" : "NaN";
	  }
	  else
	  { sprintf(buf,PL_atom_chars(status.float_fmt),d);
	    s = buf;
	  }

	  PutToken(S, s);
	  break;
	}
    case ref_tag:
    case fun_tag: // Impossible error occur
  }

  return;
}


static
int Display(pl_stream S, term_t term, w_opt *opt)
{ term_t t;

  t=deref(term);
  if (is_fun(t))
  { fun_t f=get_fun(t);
    atom_t name=f->functor;
    int   arity=f->arity;

    if (name == ATOM(comma))
      Puts(S, "','");
    else
      WriteAtom(S, name, opt);

    Putc(S, '(');
    Display(S, ++t, opt);
    for(--arity;arity>0; --arity)
    { Puts(S, ", ");
      Display(S, ++t, opt);
    }
    Putc(S, ')');
  }
  else
    WritePrimitive(S, t, opt);

  succeed;
}

static
int display(pl_stream S, term_t t, int quote)
{ w_opt opt;

  opt.max_depth		= LONG_MAX;
  opt.bind		= 0;
  opt.flags		= 0;
  if (status.char_esc)
    opt.flags |= OPT_ESC;
  if (quote)
    opt.flags |= OPT_QUOT;

  return Display(S, t, &opt);
}

#if 0
int pl_display(term_t term)
{  
  return display(OutStream(), term, 0);
}

int pl_displayq(term_t term)
{
  return display(OutStream(), term, 1);
}

int pl_display2(term_t stream, term_t term)
{ pl_stream S=Output_Stream(stream);

  if (!S) fail;
  return display(S, term, 0);
}

int pl_displayq2(term_t stream, term_t term)
{ pl_stream S=Output_Stream(stream);

  if (!S) fail;
  return display(S, term, 1);
}

int PL_display(pl_stream S, term_t t)
{
  return display(S, t, 0);
}

int PL_displayq(pl_stream S, term_t t)
{
  return display(S, t, 1);
}
#endif

static int priorityOperator(atom_t atom)
{ int type, priority;
  int result = 0;

  if (is_op(OP_PREFIX, atom, &type, &priority) && priority > result)
    result = priority;
  if (is_op(OP_POSTFIX, atom, &type, &priority) && priority > result)
    result = priority;
  if (is_op(OP_INFIX, atom, &type, &priority) && priority > result)
    result = priority;

  return result;
}

// FIXME : stuff picked from SWI-Prolog
/*  write a term. The 'enviroment' precedence is prec.
 ** Sun Apr 17 12:48:09 1988  jan@swivax.UUCP (Jan Wielemaker)
 */

inline static
bool needSpace(atom_t a1, atom_t a2)
{ if ( a1 && a2 )
  { int t1 = atomType(a1);
    int t2 = atomType(a2);

    if ( (t1 == AT_SYMBOL && t2 == AT_LOWER) ||
	 (t1 == AT_LOWER  && t1 == AT_SYMBOL) )
      fail;
  }

  succeed;
}

static
bool WriteTerm(pl_stream S, term_t t,
	int prec, int depth, const w_opt *opt)
{ atom_t functor;
  int arity;
  int op_type, op_pri;
  atom_t a;
  char short_buf[33];

  if (depth > opt->max_depth)
  { Puts(S, "...");
    succeed;
  }

  t=deref(t);

  if ((a = PL_get_atom(t)))
  { if ( priorityOperator(a) > prec )
    { PutOpenBrace(S);
      WriteAtom(S, a, opt);
      Putc(S, ')');
    }
    else
      WriteAtom(S, a, opt);

    succeed;
  }

  if ( !PL_get_name_arity(t, &functor, &arity) )
  { WritePrimitive(S, t, opt);
    succeed;
  }
  else
  { term_t arg = PL_new_term_ref();

    if (arity == 1)
    { if ( functor == ATOM(curl) &&	/* {a,b,c} */
           Options(OPT_CURL) )
      { term_t a = PL_new_term_ref();

	PL_get_arg(1, t, arg);
	Putc(S, '{');
	for(;;)
	{ if ( !PL_is_functor(arg, FUN(comma,2)) )
	    break;
	  PL_get_arg(1, arg, a);
	  WriteTerm(S, a, 999, depth+1, opt);
	  Puts(S, ", ");
	  PL_get_arg(2, arg, arg);
	}
	WriteTerm(S, arg, 999, depth+1, opt);      
	Putc(S, '}');

	succeed;
      }

      if ( functor == ATOM(numbervar) &&	/* $VAR/1 */
	   Options(OPT_NUMV) )
      { int n;

        PL_get_arg(1, t, arg);
        if ( PL_get_integer(arg, &n) && n >= 0 )
        { int i = n % 26;
          int j = n / 26;

          PutOpenToken(S, '0');
          Putc(S, i+'A');
          if ( j > 0 )
          { sprintf(short_buf, "%d", j);
            Puts(S, short_buf);
          }
          succeed;
        }
      }

      if ( functor == ATOM(namevar) &&	/* $VAR/1 */
	   Options(OPT_NAMV) )
      { atom_t a;

        PL_get_arg(1, t, arg);
        if (( a = PL_get_atom(arg) ))
        { w_opt opt2;
          memcpy(&opt2, opt, sizeof(w_opt));
          opt2.flags &= ~OPT_QUOT;

          WriteAtom(S, a, &opt2);

          succeed;
        }
      }

      if (Options(OPT_OPS))			/* op <term> */
      { if ( is_op(OP_PREFIX, functor, &op_type, &op_pri) )
        { term_t arg = PL_new_term_ref();
          int pri;
  
  	  PL_get_arg(1, t, arg);
  	  if ( op_pri > prec )
  	    PutOpenBrace(S);
  	  WriteAtom(S, functor, opt);
  	  if (op_type == OP_FX)
            pri = op_pri-1;
          else
            pri = op_pri;
  	  WriteTerm(S, arg, pri, depth+1, opt);
  	  if ( op_pri > prec )
  	    Putc(S, ')');
  
  	  succeed;
        }
  
  					/* <term> op */
        if ( is_op(OP_POSTFIX, functor, &op_type, &op_pri) )
        { term_t arg = PL_new_term_ref();
          int pri;
  
  	  PL_get_arg(1, t, arg);
  	  if ( op_pri > prec )
  	    PutOpenBrace(S);
  	  if (op_type == OP_XF)
            pri = op_pri-1;
          else
            pri = op_pri;
  	  WriteTerm(S, arg, pri, depth+1, opt);
  	  WriteAtom(S, functor, opt);
  	  if (op_pri > prec)
  	    Putc(S, ')');
  
  	  succeed;
        }
      }
    }
    else
    if ( arity == 2 )
    { if ( functor == ATOM(dot) &&	/* [...] */
           Options(OPT_LIST) )
      { Putc(S, '[');
        for(;;)
        { WriteTerm(S, t+1, 999, depth+1, opt);
	  t=deref(t+2);

	  if (is_nil(t)) 
	    break;
	  if (!is_cons(t))
	  { Putc(S, '|');
	    WriteTerm(S, t, 999, depth+1, opt);
	    break;
	  }
	  Puts(S, ", ");
	}
	Putc(S, ']');

	succeed;
      }

      if (Options(OPT_OPS))		/* <term> op <term> */
      { if ( is_op(OP_INFIX, functor, &op_type, &op_pri) )
        { term_t a = PL_new_term_ref();
          int pri;

	  if ( op_pri > prec )
	    PutOpenBrace(S);
	  PL_get_arg(1, t, a);
          if (op_type==OP_XFX || op_type == OP_XFY)
            pri = op_pri-1;
          else
	    pri = op_pri; 
	  WriteTerm(S, a, pri, depth+1, opt);
	  WriteAtom(S, functor, opt);
	  if ( functor == ATOM(comma) )
	    Putc(S, ' ');
	  PL_get_arg(2, t, a);
          if (op_type==OP_XFX || op_type == OP_YFX)
            pri = op_pri-1;
          else
	    pri = op_pri; 
	  WriteTerm(S, a, pri, depth+1, opt);
	  if ( op_pri > prec )
	    Putc(S, ')');
	  succeed;
        }
      }
    }

					/* functor(<args> ...) */
    { WriteAtom(S, functor, opt);
      Putc(S, '(');
      WriteTerm(S, ++t, 999, depth+1, opt);
      for(--arity; arity>0; --arity)
      { Puts(S, ", ");
	WriteTerm(S, ++t, 999, depth+1, opt);
      }
      Putc(S, ')');
    }
  }

  succeed;
}

inline static
int writeTerm(pl_stream S, term_t t,
		int prec, int quote, int display)
{ w_opt opt;
  
  opt.bind	= 0;
  opt.max_depth	= LONG_MAX;
  if (!display)
    opt.flags	= OPT_OPS | OPT_LIST;
  else
    opt.flags	= 0;
  if (status.char_esc)
    opt.flags |= OPT_ESC;
  if (quote)
    opt.flags |= OPT_QUOT;

  lastc = -1;
  return WriteTerm(S, t, prec, 0, &opt);
}

int PL_write(pl_stream S, term_t t)
{ return(writeTerm(S, t, 1200, 0, 0)); }

int PL_writeq(pl_stream S, term_t t)
{
  return(writeTerm(S, t, 1200, 1, 0));
}

int pl_write(term_t t)
{ writeTerm(OutStream(), t, 1200, 0, 0);
  succeed;
}

int pl_write_ln(term_t t)
{ pl_stream S=OutStream();

  writeTerm(S, t, 1200, 0, 0);
  Sputc(S,'\n');
  succeed;
}

int pl_report(term_t t)
{ writeTerm(Stderr, t, 1200, 0, 0);
  Sputc(Stderr,'\n');
  succeed;
}

int pl_writeq(term_t t)
{ writeTerm(OutStream(), t, 1200, 1, 0);
  succeed;
}

int pl_write2(term_t stream, term_t t)
{ pl_stream S=Output_Stream(stream);

  if (!S) fail;
  writeTerm(S, t, 1200, 0, 0);
  succeed;
}

int pl_writeq2(term_t stream, term_t term)
{ pl_stream S=Output_Stream(stream);

  if (!S) fail;
  writeTerm(S, term, 1200, 1, 0);
  succeed;
}

int pl_display(term_t t)
{ writeTerm(OutStream(), t, 1200, 0, 1);
  succeed;
}

int pl_displayq(term_t t)
{ writeTerm(OutStream(), t, 1200, 1, 1);
  succeed;
}

int pl_display2(term_t stream, term_t term)
{ pl_stream S=Output_Stream(stream);

  if (!S) fail;
  writeTerm(S, term, 1200, 0, 1);
  succeed;
}

int pl_displayq2(term_t stream, term_t term)
{ pl_stream S=Output_Stream(stream);

  if (!S) fail;
  writeTerm(S, term, 1200, 1, 1);
  succeed;
}

int PL_display(pl_stream S, term_t t)
{ return writeTerm(S, t, 1200, 0, 1);
}

int PL_displayq(pl_stream S, term_t t)
{ return writeTerm(S, t, 1200, 1, 1);
}


int PL_puts(char *s)
{ pl_stream S=OutStream();
  Sputs(S,s);
  succeed;
}


#include "pl-atom.h"

int pl_warn(const char *fmt)
{ static char buf[2048];
  term_t term;

  sprintf(buf, "[Warning: %s ]\n", fmt);
  term=(term_t)lookup_atom(buf);
  writeTerm(Stderr, term, 1200, 0, 0);
  Sputc(Stderr,'\n');

  succeed;
}

#include "pl-option.h"


static
int get_options(term_t Options, w_opt *options, const char *pred)
{ static int    opt_quoted;
  static int    opt_char_esc;
  static int    opt_ignore_ops;
  static int    opt_numbervars;
  static int    opt_namevars;
  static term_t opt_bindvars;
  static int    opt_space;
  static int    opt_list;
  static int    opt_curly;
  static long   opt_max_depth;
  static pl_opt_spec_t specs[] =
  { { ATOM(_quoted), OPT_BOOL, { .bool = &opt_quoted} },
    { ATOM(_char__escape), OPT_BOOL, { .bool = &opt_char_esc} },
    { ATOM(_ignore__ops), OPT_BOOL, { .bool = &opt_ignore_ops} },
    { ATOM(_numbervars), OPT_BOOL, { .bool = &opt_numbervars} },
    { ATOM(_namevars), OPT_BOOL, { .bool = &opt_namevars} },
    { ATOM(_bindvars), OPT_TERM, { .term = &opt_bindvars} },
    { ATOM(_space__args), OPT_BOOL, { .bool = &opt_space} },
    { ATOM(_list__notation), OPT_BOOL, { .bool = &opt_list} },
    { ATOM(_curly__notation), OPT_BOOL, { .bool = &opt_curly} },
    { ATOM(_max__depth), OPT_INTG, { .intg = &opt_max_depth} },
    { 0, 0, { 0 } }
  };
  
// Set default value;
  opt_quoted = 0;
  opt_char_esc = status.char_esc;
  opt_ignore_ops = 0;
  opt_numbervars = 0;
  opt_namevars = 0;
  opt_bindvars = 0;
  opt_space = 0;
  opt_list = 0;
  opt_curly = 0;
  opt_max_depth = LONG_MAX;

// Scan the options
  if (!scan_options(Options, specs))
  { PL_warning("%s : illegal option list", pred);
  }

// Process the options
  options->flags = 0;
  options->bind  = opt_bindvars;
  options->max_depth = opt_max_depth;

  if (opt_quoted)	options->flags |= OPT_QUOT;
  if (opt_char_esc)	options->flags |= OPT_ESC;
  if (opt_ignore_ops == 0)
  { options->flags |= OPT_OPS;
    options->flags |= OPT_LIST;
  }
  if (opt_numbervars)	options->flags |= OPT_NUMV;
  if (opt_namevars)	options->flags |= OPT_NAMV;
  if (opt_space)	options->flags |= OPT_SPAC;
  if (opt_list)		options->flags |= OPT_LIST;
  if (opt_curly)	options->flags |= OPT_CURL;

  succeed;
}

static
int PL_write_term(pl_stream S,term_t term, term_t options,
	const char *pred)
{ w_opt opt;

  if (!get_options(options, &opt, pred))
    fail; 
  
  lastc = -1;
  return WriteTerm(S, term, 1200, 0, &opt);
}

int pl_write_term(term_t term, term_t options)
{ pl_stream S = OutStream();

  return PL_write_term(S,term, options, "write_term/2");
}

int pl_write_term3(term_t stream, term_t term, term_t options)
{ pl_stream S=Output_Stream(stream);

  if (!S) fail;
  return PL_write_term(S,term, options, "write_term/3");
}
