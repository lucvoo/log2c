/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <termios.h>
#include <sys/stat.h>
#include <unistd.h>
#include <pwd.h>
#include <errno.h>
#include "pl-os.h"
#include "pl-ctype.h"
#include "pl-fli.h"
#include "pl-string.h"

#define PL_msg(M)


// Return a string describing the last OS call error
char *PL_OsError(void)
{ return(strerror(errno));
}

int PL_System(const char *cmd)
{ return(system(cmd));		// FIXME : close I/O       ?
				//         signal handling ?
}

/**********************************************************************/
/* Time stuff                                                         */
/**********************************************************************/

static double clk_tck;

// Return the amount of second of CPU time (user + system time)
// used by the process.
double PL_CpuTime(void)
{ struct tms t;

  times(&t);
  return((t.tms_utime+t.tms_stime)/clk_tck);
}


// Wrapper for localtime
struct tm* PL_LocalTime(long t)
{ time_t T=t;
  return(localtime(&T));
}


/**********************************************************************/
/* Random Numbers stuff                                               */
/**********************************************************************/

// Return a Random Number (in the range ... )
unsigned long PL_Random(void)
{ return(rand());
}

static void InitRandom(void)
{ srand(time(0));
}


/**********************************************************************/
/* File Management                                                    */
/**********************************************************************/

inline static const char *EnsureAbsolutePath(const char *path)
{ if (path[0]=='/')
    return(path);
  else
  { static char buf[PATH_MAX+1], *cwd;
    int len;
    cwd=getcwd(buf,sizeof(buf)-1);
    if (cwd!=buf)
      return(0);		// FIXME : msg ?
    len=strlen(buf);
    buf[len]='/';
    strcpy(buf+(len+1),path);
    return(buf);
  }
}

char *PL_ReadLink(const char *path)
{ static char buf[PATH_MAX+1];
  int n;

  if ( (n=readlink(path,buf,sizeof(buf)-1)) >= 0 )
  { buf[n]='\0';		// readlink doesn't add the null char
    return(buf);
  }
  else
    return(0);
}

/**********************************************************************/
/* FileName Expansion (tilde only !)                                  */
/**********************************************************************/

#include <pwd.h>
#include <sys/types.h>

// if <user> is null
//   return the home directory of the real user of this process
// else
//   return the home directory of <user>
static char *PL_GetHome(const char *user)
{ struct passwd *pw;

  if (!user)
  { char *home;

    if ((home=getenv("HOME")))
      return(home);
    else
      pw=getpwuid(getuid());
  }
  else
    pw=getpwnam(user);

  return(pw ? pw->pw_dir : 0);
}



// expand '~[user]' in pathname
// return expanded path or null if error
inline static char *ExpandTilde(const char *path)
{ static char new_path[PATH_MAX+1];
  char *np=new_path;
  int len=0;

  if (path[0]=='~')	// must do tilde expansion
  { const char *home;

    path++;
    if (path[0]=='/' || path[0]=='\0')	// get real user home directory
    { home = PL_GetHome(0);
    }
    else				// get home directory of user
    { char *user;
      char *stop;

      stop=strchr(path,'/');
      len=( stop ? stop-path : strlen(path) );
      if (!(user=alloca(len+1)))
      { PL_msg("alloca failed");
        return(0);
      }

      strncpy(user,path,len);
      user[len]='\0';

      if ((home = PL_GetHome(user)))
        path+=len;
      else
      { home="~"; }
    }

    len=strlen(home);
    if (len>=sizeof(new_path))
    { PL_msg("Pathname too long");
      return(0);
    }

    strcpy(new_path,home);
    np=new_path+len;
  }

  if ((len+strlen(path))>=sizeof(new_path))
  { PL_msg("Pathname too long");
    return(0);
  }
  strcpy(np,path);
  return(new_path);
}



char *PL_CanonicalPath(const char *path, char canon_path[])
{ char *p;

  if ((p=ExpandTilde(path)))
    path=p;

#ifdef HAVE_REALPATH
  if ((p=realpath(path,canon_path)))
    return(p);
  else
#endif
    return((char *) path);
}



// For the moment interprete only '~' and '~<user>' construct.
char *PL_ExpandFile(const char *file, char *expanded)
{ static char buf[PATH_MAX+1];
  char *x;

  if (!expanded)
    expanded=buf;

  if (!(x=ExpandTilde(file)))
  { x=(char *) file;
  }

  strcpy(expanded,file);
  return(expanded);
}


int PL_AccessFile(const char *path, int mode)
{ int m = 0;

  if ( mode == PL_ACCESS_EXIST )
    m = F_OK;
  else
  { if ( mode & PL_ACCESS_READ    ) m |= R_OK;
    if ( mode & PL_ACCESS_WRITE   ) m |= W_OK;
    if ( mode & PL_ACCESS_EXECUTE ) m |= X_OK;
  }

  return(!access(path, m));
}


int PL_ExistsFile(const char *path)
{ if (access(path, F_OK) == 0 )
    succeed;
  fail;
}


int PL_ExistsDirectory(const char *path)
{ struct stat buf;

  if (stat(path, &buf))
    fail;
  if ((buf.st_mode & S_IFMT) == S_IFDIR)
    succeed;
  fail;
}


long PL_SizeFile(const char *path)
{ struct stat buf;

  if (stat(path, &buf))
    return(-1);

  return(buf.st_size);
}


/**********************************************************************/
/* Terminal Handling                                                  */
/**********************************************************************/

// static struct termios	initial_termios;
static struct termios	save_termios;
static int				ttysavefd = -1;
static enum { RESET, RAW, CBREAK }	ttystate = RESET;

static int tty_cbreak(int fd)	/* put terminal into a cbreak mode */
{ struct termios  buf;

  if (tcgetattr(fd, &save_termios) < 0)
    return(-1);
  if (tcgetattr(fd, &buf) < 0)
    return(-1);

  buf.c_lflag &= ~(ECHO | ICANON); /* echo off, canonical mode off */

  buf.c_cc[VMIN] = 1;  /* Case B: 1 byte at a time, no timer */
  buf.c_cc[VTIME] = 0;

  if (tcsetattr(fd, TCSAFLUSH, &buf) < 0)
    return(-1);
  ttystate = CBREAK;
  ttysavefd = fd;
  return(0);
}

static int tty_reset(int fd)		/* restore terminal's mode */
{ if (ttystate != CBREAK && ttystate != RAW)
    return(0);

  if (tcsetattr(fd, TCSAFLUSH, &save_termios) < 0)
    return(-1);
  ttystate = RESET;
  return(0);
}

static void tty_atexit(void)    /* can be set up by atexit(tty_atexit) */
{ if (ttysavefd >= 0)
    tty_reset(ttysavefd);
}


int PL_GetSingleChar(void)
{ int c;

  if (isatty(STDIN_FILENO))
  { if ( tty_cbreak(STDIN_FILENO) ||
         (read(STDIN_FILENO,&c,1)!=1) ||
         tty_reset(STDIN_FILENO) )
      return(-1);
  }
  else
  { if (read(STDIN_FILENO,&c,1)!=1)
      return(-1);
  }

  return(c);
}

/**********************************************************************/
/* environment stuff                                                  */
/**********************************************************************/

int PL_setenv(const char *name, const char *val)
{ int name_len=strlen(name);
  char *buf=alloca(name_len+1+strlen(val)+1);

  strcpy(buf,name);
  buf[name_len]='=';
  strcpy(buf+name_len+1,val);
  if (putenv(buf)!=0)
    PL_warning("setenv/2: os error");	// FIXME : use errno for error ?

  succeed;
}

void PL_unsetenv(const char *name)
{ unsetenv(name);
}

/**********************************************************************/
/* ...                                                                */
/**********************************************************************/

#include "pl-init.h"

void PL_init_os(void)
{ InitRandom();
  atexit(tty_atexit);
  clk_tck=sysconf(_SC_CLK_TCK);
}

