/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "Prolog.h"
#include "pl-ctype.h"
#include "pl-stream.h"
#include "pl-fli.h"


int PL_display(term_t, pl_stream);
int PL_displayq(term_t, pl_stream);
int PL_write(term_t, pl_stream);
int PL_writeq(term_t, pl_stream);
pl_stream OutStream(void);
pl_stream Output_Stream(term_t stream);


#define BUFSIZE 	10240
#define DEFAULT 	(-1)
#define SHIFT   	{ ++argv; }
#define NEED_ARG	{ if (!argv->celp) \
			  { ERROR("not enough arguments"); \
			  } \
			}
#define ERROR(fmt)	PL_warning("format/2: " fmt)
#define ERROR1(fmt, a)	PL_warning("format/2: " fmt, a)

#define OUTSTRING(s)	Sputs(S,s)
#define OUTCHR(c)	Sputc(S,c)


		/********************************
		*       ACTUAL FORMATTING	*
		********************************/

inline static
int update_column(int col, int c)
{ switch(c)
  { case '\n':	return 0;
    case '\t':	return (col + 1) | 0x7;
    case '\b':	return (col <= 0 ? 0 : col - 1);
    default:	return col + 1;
  }
}   


static bool
do_format(const char *fmt, term_t argv, pl_stream S)
{
  while(*fmt)
  { if (*fmt=='~')
    { int arg = DEFAULT;		/* Numeric argument */
      				/* Get the numeric argument */
      if ( isDigit(*++fmt) )
      { for( ; isDigit(*fmt); fmt++ )
          arg = (arg == DEFAULT ?
                 arg = *fmt - '0' :
                 arg*10 + *fmt - '0');
      }
      else
      if ( *fmt == '*' )
      { NEED_ARG;
        if ( PL_get_integer(argv, &arg) && arg >= 0)
        { SHIFT;
        }
        else
          ERROR("no or negative integer for `*' argument");
        fmt++;
      }
      else
      if ( *fmt == '`' )
      { arg = *++fmt;
        fmt++;
      }
        
      switch(*fmt)		/* Build in formatting */
      { case 'a':  { const char *s;   /* Atomic */

                     NEED_ARG;
                     if ( !PL_get_chars(argv, &s, CVT_ATOMIC) )
                       ERROR("illegal argument to ~a");
                     SHIFT;
                     Sputs(S,s);
                     fmt++;
                    break;
                   }
        case 'c':  { int c;	/* ascii */
           
                     NEED_ARG;
                     if ( PL_get_integer(argv, &c) && c>=0 && c<=255 )
                     { int times = (arg == DEFAULT ? 1 : arg);
           
                       SHIFT;
                       while(times-- > 0)
                       { OUTCHR(c);
                       }
                     }
                     else
                       ERROR("illegal argument to ~c");
                     fmt++;
                     break;
                   }
        case 'd':			/* integer */
        case 'D':			/* grouped integer */
        case 'r':			/* radix number */
        case 'R':			/* Radix number */
                   { int i;
                     char *s;
           
                     NEED_ARG;
                     if ( !PL_get_integer(argv, &i) )
                       ERROR1("illegal argument to ~%c", *fmt);
                     SHIFT;
                     if ( arg == DEFAULT )
                       arg = 0;
                     if (*fmt == 'd' || *fmt == 'D')
                       s=formatInteger(*fmt == 'D', arg, 10, TRUE, i);
                     else
                       s=formatInteger(FALSE, 0, arg, *fmt == 'r', i);

                     Sputs(S,s);			
                     fmt++;
                     break;
                   }
        case 's':			/* string */
                   { const char *s;
           
                     NEED_ARG;
                     if ( !PL_get_list_codes(argv, &s, BUF_DISCARDABLE) )
                       ERROR("illegal argument to ~s");
                     Sputs(S,s);
                     SHIFT;
                     fmt++;
                     break;
                   }
        case 'i':			/* ignore */
                   { NEED_ARG;
                     SHIFT;
                     fmt++;
                     break;
                   }
                   { int (*f)(term_t, pl_stream);
        case 'k':    			/* displayq */
                     f=PL_displayq;
                     goto pl_common;
        case 'q':			/* writeq */
                     f=PL_writeq;
                     goto pl_common;
        case 'w':			/* write */
                     f=PL_write;

        pl_common:   NEED_ARG;
                     (*f)(argv,S);
                     SHIFT;
                     fmt++;
                     break;
                   }
        case '~':			/* ~ */
                   { OUTCHR('~');
                     fmt++;
                     break;
                   }
        case 'n':			/* \n */
        case 'N':			/* \n if not on newline */
                   { if ( arg == DEFAULT )
                       arg = 1;
                     // if ( *fmt == 'N' && column == 0 )  arg--;
                     while( arg-- > 0 )
                       OUTCHR('\n');
                     fmt++;
                     break;
                   }
      }
    }
    else
//    if (*fmt=='%')
//    {
//    }
//    else
    { OUTCHR(*fmt);
      fmt++;
    }
  }

  succeed;
}

static cell_t end_cell = { .celp = 0 };
static term_t empty_tab = &end_cell;


inline static
term_t list_to_tab(term_t list)
{ int n=0;
  term_t l;

  l=deref(list);
  while (is_cons(l))
  { HP[n].celp=l+1;
    l=deref(l+2);
    n++;
  }
  if (!is_nil(l))
  { HP[0].celp=list;
    n=1;
  }
  HP[n]=end_cell;
  return(HP);
}

int pl_format(term_t fmt, term_t args)
{ const char *f;

  if (!PL_get_chars(fmt, &f, CVT_ALL|BUF_RING))
    PL_warning("format/2: format is not an atom or string");

  return(do_format(f, list_to_tab(args), OutStream()));
}

int pl_format3(term_t stream, term_t fmt, term_t args)
{ const char *f;
  pl_stream S=Output_Stream(stream);

  if (!PL_get_chars(fmt, &f, CVT_ALL|BUF_RING))
    PL_warning("format/2: format is not an atom or string");

  return(do_format(f, list_to_tab(args), S));
}


int pl_sformat3(term_t string, term_t fmt, term_t args)
{ const char *f, *s;
  int rval;
  pl_stream S=Sopen_wmem(0, SM_WRITE, 0);

  if (!PL_get_chars(fmt, &f, CVT_ALL|BUF_RING))
  { Sclose(S);
    PL_warning("format/2: format is not an atom or string");
  }

  rval=do_format(f, list_to_tab(args), S);
  s=Sstring_wmem(S);
  Sclose(S);
  if (!rval)
    fail;
  else
    return(PL_unify_atom_chars(string,s)); 
}

int pl_sformat2(term_t string, term_t fmt)
{ const char *f, *s;
  int rval;
  pl_stream S=Sopen_wmem(0, SM_WRITE, 0);

  if (!PL_get_chars(fmt, &f, CVT_ALL|BUF_RING))
  { Sclose(S);
    PL_warning("format/2: format is not an atom or string");
  }

  rval=do_format(f, empty_tab, S);
  s=Sstring_wmem(S);
  Sclose(S);
  if (!rval)
    fail;
  else
    return(PL_unify_atom_chars(string,s)); 
}

