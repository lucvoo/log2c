/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "Prolog.h"
#include "pl-status.h"
#include "pl-atom.h"

status_t status;

#define	pl_flags_size	16

typedef enum { T_VOID, T_ATOM, T_INTG, T_BOOL } pf_type;

#define OFF	0
#define ON	1

#ifndef	TRUE
#define FALSE	0
#define TRUE	1
#endif

typedef struct pflag_t *pflag_t;
struct pflag_t { atom_t  key;
                 cell_t  val;
                 union { int *intg; atom_t *atom; } addr;
                 int     lock;
                 pf_type type;
                 pflag_t next;
               };

static pflag_t pl_flags[pl_flags_size];


inline static
pflag_t lookup_pflag(atom_t key, int new)
{ pflag_t f;
  hash_t h;

  h=key->hash % pl_flags_size;

  for (f=pl_flags[h]; f; f=f->next)
     if (key==f->key)
       return(f);		// find flag.

  if (new)
  { f=NEW(*f);			// create new flag
    f->key=key;			// with this keys.
    f->lock=0;
    f->addr.atom=0;
    f->type=T_VOID;
    f->next=pl_flags[h];		// insert this flag in the table
    pl_flags[h]=f;
    return(f);
  }
  else
    return(0);			// inexistant flag
}
          

inline static
int SetAtom(pflag_t f, atom_t val, int lock, atom_t *addr)
{ f->type=T_ATOM;
  if (lock) f->lock=1;
  if (addr) f->addr.atom=addr;
  if (f->addr.atom)
    *(f->addr.atom)=val;
  else
    f->val.celp=&(val->atom);
  succeed;
}

inline static
int Setpf_atom(const char *key, atom_t val, int lock, atom_t *addr)
{ pflag_t f;

  if ((f=lookup_pflag(lookup_atom(key),1)))
    return SetAtom(f,val,lock,addr);
  else
    fail;
}

inline static
int Setpf_str(const char *key, const char *val, int lock, atom_t *addr)
{ return(Setpf_atom(key,lookup_atom(val), lock, addr)); }


inline static
int SetInt(pflag_t f, long val, int lock, int *addr, int type)
{ f->type=type;
  if (lock) f->lock=1;
  if (addr) f->addr.intg=addr;
  if (f->addr.intg)
    *(f->addr.intg)=val;
  else
    f->val.val=__intg(val);
  succeed;
}

inline static
int Setpf_int(const char *key, long val, int lock, int *addr)
{ pflag_t f;

  if ((f=lookup_pflag(lookup_atom(key),1)))
    return SetInt(f,val,lock,addr,T_INTG);
  else
    fail;
}

inline static
int Setpf_boo(const char *key, long val, int lock, int *addr)
{ pflag_t f;

  if ((f=lookup_pflag(lookup_atom(key),1)))
    return SetInt(f,val,lock,addr,T_BOOL);
  else
    fail;
}


int pl_set_prolog_flag(term_t key, term_t new)
{ pflag_t f;
  atom_t k;

  if (!(k = PL_get_atom(key)))
    fail;

  if (!(f=lookup_pflag(k,0)))
    fail;
  
  if (f->lock)
    fail;		// flag is locked 
  
  new=deref(new);
  switch(get_tag(new))
  { case ref_tag: fail; 	// impossible error
    case ato_tag: if (f->type==T_BOOL)
                  { if (isatom(ATOM(_true),new))
                      return(SetInt(f,TRUE,0,0,T_BOOL));
                    else
                    if (isatom(ATOM(_false),new))
                      return(SetInt(f,FALSE,0,0,T_BOOL));
                    else
                      fail;
                  }
                  else
                    return(SetAtom(f,get_atom(new),0,0));
    case int_tag: return(SetInt(f,get_val(new),0,0,T_INTG));
    default:      fail;
  }
}

int pl_prolog_flag(term_t key, term_t old, term_t new)
{ pflag_t f;
  atom_t k;

  if (!(k = PL_get_atom(key)))
    fail;

  if ((f=lookup_pflag(k,0)))
  { switch(f->type)
    { case T_ATOM: { atom_t a= f->addr.atom ? *(f->addr.atom) : (atom_t) f->val.celp;
                     if (PL_unify_atom(old,a))
                       break;
                     else
                       fail;
                   }
                   { int i;
      case T_BOOL:   i= f->addr.intg ? *(f->addr.intg) : f->val.tag_val.val;
                     if (PL_unify_bool(old,i))
                       break;
                     else
                       fail;
      case T_INTG:   i= f->addr.intg ? *(f->addr.intg) : f->val.tag_val.val;
                     if (PL_unify_intg(old,i))
                       break;
                     else
                       fail;
                   }
      default:     fail;
    }
  }
  else
  if (!is_var(deref(old)) || !(f=lookup_pflag(k,1)))
    fail;
  
  if (!new) succeed;

  if (f->lock) fail;		// flag is locked 
  
  new=deref(new);
  switch(get_tag(new))
  { case ref_tag: fail; 	// impossible error
    case ato_tag: if (f->type==T_BOOL)
                  { if (isatom(ATOM(_true),new))
                      return SetInt(f,1,0,0,T_BOOL);
                    else
                    if (isatom(ATOM(_false),new))
                      return SetInt(f,0,0,0,T_BOOL);
                    else
                      fail;
                  }
                  else
                    return(SetAtom(f,(atom_t) new,0,0));
    case int_tag: return(SetInt(f,get_val(new),0,0,T_INTG));
    default:      fail;
  }
}

int pl_prolog_flag_2(term_t key, term_t val)
{ return pl_prolog_flag(key,val,0);
}

// #####################################################################

#include "pl-init.h"

void init_prolog_flag(void)
{
/* ISO prolog-flags */
  Setpf_int("bounded",		TRUE,		1,0);
  Setpf_boo("char_conversion",	FALSE,		0,&status.char_conv);
  Setpf_boo("debug",		FALSE,		0,&status.debug);
  Setpf_str("double_quotes",	"codes",	0,&status.dbl_quotes);
if ((-3/2)==-2)
  Setpf_str("integer_rounding_function","down",	1,0);
else
  Setpf_str("integer_rounding_function","toward_zero",1,0);
  Setpf_int("max_arity",	PL_MAX_INT,	1,0);
  Setpf_int("max_integer",	PL_MAX_INT,	1,0);
  Setpf_int("min_integer",	PL_MIN_INT,	1,0);
  Setpf_str("unknown",		"fail",		1,0);

  Setpf_str("back_quotes",	"codes",	0,&status.bck_quotes);

/* SWI flags (please, features, unknow, style_check, fileerrors) */
  Setpf_str("arch",		PL_ARCH,	1,0);
  Setpf_str("c_cc",		CC,		1,0);
  Setpf_str("c_ldflags",	C_LD_FLAGS,	1,0);
  Setpf_str("c_libs",		C_LIBS,		1,0);
  Setpf_str("c_options",	C_OPTIONS,	1,0);
  Setpf_str("c_staticlibs",	C_STATIC_LIBS,	1,0);
  Setpf_boo("character_escapes",TRUE,		0,&status.char_esc);
  Setpf_str("compiled_at",	__DATE__ ", " __TIME__,1,0);
  Setpf_boo("dynamic_stacks",	TRUE,		1,0);
  Setpf_str("float_format",	"%g",		0,&status.float_fmt);
  Setpf_boo("gc",		FALSE,		1,0);
  Setpf_str("home",		PL_HOME,	1,0);
  Setpf_boo("iso",		FALSE,		1,&status.iso);
  Setpf_int("max_tagged_integer",PL_MAX_TAG_INT,1,0);
  Setpf_int("min_tagged_integer",PL_MIN_TAG_INT,1,0);
  Setpf_boo("open_shared_object",FALSE,		1,0);
  Setpf_boo("pipe",		TRUE,		1,0);
  Setpf_boo("readline",		FALSE,		1,0);
  Setpf_boo("report_error",	TRUE,		0,&status.rep_err);
  Setpf_boo("tty_control",	TRUE,		0,&status.tty_ctrl);
#if defined(__unix__) || defined(unix)
  Setpf_boo("unix",		TRUE,		1,0);
#else
  Setpf_boo("unix",		FALSE,		1,0);
#endif
  Setpf_int("version",		PL_VERSION,	1,0);
 
/* SWI flags : style_check */
  Setpf_boo("discontiguous",	FALSE,		0,&status.discont);
  Setpf_boo("dollar",		FALSE,		0,&status.dollar);
  Setpf_boo("long_atom",	TRUE,		0,&status.long_atom);
  Setpf_boo("singleton",	TRUE,		0,&status.singleton);
 
/* SWI flags : fileerrors */
  Setpf_boo("file_error",	FALSE,		0,&status.file_err);

/* Own extension */
  Setpf_int("nested_comment",	TRUE,		0,&status.nested_com);
}
