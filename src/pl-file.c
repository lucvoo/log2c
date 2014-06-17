/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-fli.h"
#include "pl-os.h"

static const char *PL_get_filename(term_t file, char *buf)
{ static char buffer[PATH_MAX+1];
  const char *name;

  if (!buf)
    buf=buffer;

  if ( PL_get_chars(file,&name,CVT_ALL) &&
       (name = PL_ExpandFile(name,buf)) )
    return(name);
  else
    return(0);
}


int pl_file_base_name(term_t path, term_t base)
{ const char *p;

  if (!PL_get_atom_chars(path,&p))
    PL_warning("file_base_name/2 : instantiation fault");

  return(PL_unify_atom_chars(base,PL_BaseName(p)));
}

int pl_file_directory_name(term_t path, term_t dir)
{ const char *p;

  if (!PL_get_atom_chars(path,&p))
    PL_warning("file_directory_name/2 : instantiation fault");

  return(PL_unify_atom_chars(dir,PL_DirName(p)));
}

int pl_file_name_extension(term_t base, term_t ext, term_t full)
{ const char *b, *e, *f;
  static char buf[PATH_MAX];

  if (PL_get_atom_chars(full,&f))
  { const char *s=f+strlen(f);

    while ( *s!='.' && *s!='/' && s > f)
      s--;

    if (*s=='.')
    { if (PL_get_atom_chars(ext,&e))
      { if (e[0]=='.') e++;
        if (PL_PathCmp(e,s+1)) fail;
      }
      else
      if (!PL_unify_atom_chars(ext,s+1)) fail;

      if ((s-f)>PATH_MAX)
        PL_warning("file_name_extension/3 : file name too long");

      strncpy(buf,f,s-f);
      buf[s-f]='\0';
      return(PL_unify_atom_chars(base,buf));
    }

    if (PL_unify_atom(ext,ATOM(_)) && pl_unify(full,base))
      succeed;
    fail;
  }
  else
  if (PL_get_atom_chars(base,&b) && PL_get_atom_chars(ext,&e))
  { char *s;
    if (e[0]=='.') e++;

    // FIXME : test has_extension(b,e) ?? -> get the spec !

    if ((strlen(b)+strlen(e)+2)>PATH_MAX)
      PL_warning("file_name_extension/3 : file name too long");
    strcpy(buf,b);
    s=buf+strlen(b);
    *s++='.';
    strcpy(s,e);

    return(PL_unify_atom_chars(full,buf));
  }
  else
    PL_warning("file_name_extension/3 : instantiation fault");
}



int pl_delete_file(term_t name)
{ const char *f;

  if ( !(f = PL_get_filename(name, 0)) )
    PL_warning("delete_file/1: instantiation fault");

  return(PL_RemoveFile(f));
}

int pl_absolute_file_name(term_t file, term_t abs)
{ const char *f, *c;
  static char buf[PATH_MAX+1];

  if (!PL_get_atom_chars(file,&f))
    fail;

  c = PL_CanonicalPath(f,buf);

  return(PL_unify_atom_chars(abs,c));
}

int pl_is_absolute_file_name(term_t file)
{ const char *f;

  return(PL_get_atom_chars(file,&f) && PL_isAbsolutePath(f) );
}

int pl_access_file(term_t name, term_t mode)
{ const char *n;
  int md;
  atom_t m;

  if ( !(m=PL_get_atom(mode)) ||
       !(n=PL_get_filename(name, 0)) )
    PL_warning("access_file/2: instantiation fault");

  if ( m == ATOM(_none) )
    succeed;

  if ( m == ATOM(_write) || m == ATOM(_append) )
    md = PL_ACCESS_WRITE;
  else
  if ( m == ATOM(_read) )
    md = PL_ACCESS_READ;
  else
  if ( m == ATOM(_execute) )
    md = PL_ACCESS_EXECUTE;
  else
  if ( m == ATOM(_exist) )
    md = PL_ACCESS_EXIST;
  else
    PL_warning("access_file/2: mode: {read,write,append,execute,exist,none}");

  if ( PL_AccessFile(n, md) )
    succeed;

  if ( md == PL_ACCESS_WRITE && !PL_AccessFile(n, PL_ACCESS_EXIST) )
  { const char *dir = PL_DirName(n);
    dir=( dir ? dir : "." );
    return(PL_AccessFile(dir, md));
  }
  else
    fail;
}

int pl_exists_file(term_t file)
{ const char *f;

  if (!(f = PL_get_filename(file, 0)))
    PL_warning("exists_file/1: instantiation fault");

  return(PL_ExistsFile(f));
}


int pl_exists_directory(term_t dir)
{ const char *d;

  if (!(d = PL_get_filename(dir, 0)))
    PL_warning("exists_directory/1: instantiation fault");

  return(PL_ExistsDirectory(d));
}


