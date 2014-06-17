/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef PL_OS_H_
#define PL_OS_H_

#include "Prolog.h"
#include "pl-string.h"
#include <stdlib.h>

#include <string.h>			// for strerror()
#include <sys/times.h>			// for times()
#include <unistd.h>			// for ...

/* pl-os.c */
char *PL_OsError(void);
double PL_CpuTime(void);
struct tm *PL_LocalTime(long t);
unsigned long PL_Random(void);
char *PL_TempFileName(char *id);
char *PL_CanonicalPath(const char *path, char canon[]);
char *PL_ExpandFile(const char *file, char expanded[]);
int PL_AccessFile(const char *path, int mode);
int PL_ExistsFile(const char *path);
int PL_ExistsDirectory(const char *path);
long PL_SizeFile(const char *path);
int PL_GetSingleChar(void);
int PL_setenv(const char *name, const char *val);
void PL_unsetenv(const char *name);
char *PL_ReadLink(const char *path);
int PL_System(const char *cmd);

inline static const char *PL_BaseName(const char *p)
{
	char *b;

	b = strrchr(p, '/');
	return (b ? b + 1 : p);
}

inline static char *PL_DirName(const char *path)
{
	static char buf[PATH_MAX + 1];
	char *p;

	if ((p = strrchr(path, '/'))) {
		strncpy(buf, path, p - path);
		buf[p - path] = '\0';
		return (buf);
	} else
		fail;
}

inline static int PL_PathCmp(const char *s1, const char *s2)
{
	return (strcmp(s1, s2));
}

inline static int PL_RemoveFile(const char *path)
{
#ifdef HAVE_REMOVE
	return (remove(path) == 0);
#else
	return (unlink(path) == 0);
#endif
}

inline static int PL_isAbsolutePath(const char *path)
{
	return (path[0] == '/');
}

// definition for access_file/2
#define	PL_ACCESS_EXIST		1
#define PL_ACCESS_READ		2
#define PL_ACCESS_WRITE		4
#define PL_ACCESS_EXECUTE	8

#endif
