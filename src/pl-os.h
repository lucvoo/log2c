/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef PL_OS_H_
#define PL_OS_H_

#include "Prolog.h"
#include "pl-string.h"
#include <stdlib.h>


#include <string.h>	// for strerror()
#include <sys/times.h>	// for times()
#include <unistd.h>	// for ...

/* pl-os.c */
void PL_halt(int status);
char *OsError(void);
double CpuTime(void);
long Time(void);
struct tm *LocalTime(long t);
unsigned long Random(void);
char *TempFileName(char *id);
char *CanonicalPath(const char *path, char canon[]);
char *ExpandFile(const char *file, char expanded[]);
int AccessFile(const char *path, int mode);
int ExistsFile(const char *path);
int ExistsDirectory(const char *path);
long SizeFile(const char *path);
int pl_test(term_t file, term_t expand);
int GetSingleChar(void);
char *Getenv(const char *name);
char *Setenv(const char *name, const char *val);
void Unsetenv(const char *name);
char *ReadLink(const char *path);


INLINE_DECL
const char *BaseName(const char *p)
{ char *b;

  b=strrchr(p,'/');
  return( b ? b+1 : p);
}

INLINE_DECL
char *DirName(const char *path)
{ static char buf[PATH_MAX+1];
  char *p;

  if ((p=strrchr(path,'/')))
  { strncpy(buf,path,p-path);
    buf[p-path]='\0';
    return(buf);
  }
  else
    fail;
}

INLINE_DECL
int PathCmp(const char *s1, const char *s2)
{ return(strcmp(s1,s2)); }


// FIXME : close non-terminal related I/O in child
INLINE_DECL
int System(const char *cmd)
{ return(system(cmd)); }


INLINE_DECL
int RemoveFile(const char *path)
{
#ifdef HAVE_REMOVE
  return(remove(path) == 0 );
#else
  return(unlink(path) == 0 );
#endif
}

INLINE_DECL
int isAbsolutePath(const char *path)
{ return(path[0]=='/'); }

// definition for access_file/2
#define	PL_ACCESS_EXIST		1
#define PL_ACCESS_READ		2
#define PL_ACCESS_WRITE		4
#define PL_ACCESS_EXECUTE	8


#endif	// PL_OS_H_
