/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "Prolog.h"
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

#include "pl-stack.h"		// For stack statistics
#include "pl-init.h"		// For init_time()

static struct rusage ru_start, ru_last, ru_now;
static struct timeval tv_start, tv_last, tv_now;

void PL_init_time(void)
{ getrusage(RUSAGE_SELF, &ru_start);
  getrusage(RUSAGE_SELF, &ru_last);
  gettimeofday(&tv_start, 0);
  gettimeofday(&tv_last, 0);
}

inline static
void cp_usage(void)
{ ru_last.ru_utime.tv_sec  = ru_now.ru_utime.tv_sec;
  ru_last.ru_utime.tv_usec = ru_now.ru_utime.tv_usec;
  ru_last.ru_stime.tv_sec  = ru_now.ru_stime.tv_sec;
  ru_last.ru_stime.tv_usec = ru_now.ru_stime.tv_usec;
}

inline static
void cp_timev(void)
{ tv_last.tv_sec  = tv_now.tv_sec;
  tv_last.tv_usec = tv_now.tv_usec;
}

int pl_statistics(term_t key, term_t pair)
{ struct pl_stack_stat stack_stat;
  term_t p;
  int d1, d2;

  Deref(key);

  if (!isAtom(key))
  { // FIXME : error msg
    fail;
  }

  getrusage(RUSAGE_SELF, &ru_now);
  gettimeofday(&tv_now, 0);
  PL_get_stack_stat(&stack_stat);

  if (isatom(ATOM(_user__time), key) || isatom(ATOM(_runtime), key))
  { d1  = ( ru_now.ru_utime.tv_sec  - ru_start.ru_utime.tv_sec) * 1000;
    d1 += ( ru_now.ru_utime.tv_usec - ru_start.ru_utime.tv_usec) /1000;
    d2  = ( ru_now.ru_utime.tv_sec  - ru_last.ru_utime.tv_sec) * 1000;
    d2 += ( ru_now.ru_utime.tv_usec - ru_last.ru_utime.tv_usec) /1000;
  }
  else
  if (isatom(ATOM(_system__time), key))
  { d1  = ( ru_now.ru_stime.tv_sec  - ru_start.ru_stime.tv_sec) * 1000;
    d1 += ( ru_now.ru_stime.tv_usec - ru_start.ru_stime.tv_usec) /1000;
    d2  = ( ru_now.ru_stime.tv_sec  - ru_last.ru_stime.tv_sec) * 1000;
    d2 += ( ru_now.ru_stime.tv_usec - ru_last.ru_stime.tv_usec) /1000;
  }
  else
  if (isatom(ATOM(_cpu__time), key))
  { d1  = ( ru_now.ru_utime.tv_sec  - ru_start.ru_utime.tv_sec) * 1000;
    d1 += ( ru_now.ru_utime.tv_usec - ru_start.ru_utime.tv_usec) /1000;
    d2  = ( ru_now.ru_utime.tv_sec  - ru_last.ru_utime.tv_sec) * 1000;
    d2 += ( ru_now.ru_utime.tv_usec - ru_last.ru_utime.tv_usec) /1000;
    d1 += ( ru_now.ru_stime.tv_sec  - ru_start.ru_stime.tv_sec) * 1000;
    d1 += ( ru_now.ru_stime.tv_usec - ru_start.ru_stime.tv_usec) /1000;
    d2 += ( ru_now.ru_stime.tv_sec  - ru_last.ru_stime.tv_sec) * 1000;
    d2 += ( ru_now.ru_stime.tv_usec - ru_last.ru_stime.tv_usec) /1000;
  }
  else
  if (isatom(ATOM(_real__time), key))
  { d1  = ( tv_now.tv_sec  - tv_start.tv_sec) * 1000;
    d1 += ( tv_now.tv_usec - tv_start.tv_usec) /1000;
    d2  = ( tv_now.tv_sec  - tv_last.tv_sec) * 1000;
    d2 += ( tv_now.tv_usec - tv_last.tv_usec) /1000;
    d1 += ( tv_now.tv_sec  - tv_start.tv_sec) * 1000;
    d1 += ( tv_now.tv_usec - tv_start.tv_usec) /1000;
    d2 += ( tv_now.tv_sec  - tv_last.tv_sec) * 1000;
    d2 += ( tv_now.tv_usec - tv_last.tv_usec) /1000;
  }
  else
  if (isatom(ATOM(_local__stack), key))
  { d1 = stack_stat.used.local;
    d2 = stack_stat.free.local;
  }
  else
  if (isatom(ATOM(_heap__stack), key))
  { d1 = stack_stat.used.heap;
    d2 = stack_stat.free.heap;
  }
  else
  if (isatom(ATOM(_sheap__stack), key))
  { d1 = stack_stat.used.sheap;
    d2 = stack_stat.free.sheap;
  }
  else
  if (isatom(ATOM(_trail__stack), key))
  { d1 = stack_stat.used.trail;
    d2 = stack_stat.free.trail;
  }
  else
    fail;	// FIXME : implement real_time

  p=HP; HP+=5;
  p[0].val=__cons();
  p[1].val=__intg(d1);
  p[2].val=__cons();
  p[3].val=__intg(d2);
  p[4].val=__nil();

  return(pl_unify(p,pair));
}


static
int _pl_time(term_t t, struct tm *tm)
{ term_t dt;

  if (!tm)
  { // FIXME error msg
    fail;
  }
  
  dt = HP; HP+=7;
  dt[0].val = __fun(FUN(_dt, 6));
  dt[1].val = __intg(tm->tm_year + 1900);
  dt[2].val = __intg(tm->tm_mon);
  dt[3].val = __intg(tm->tm_mday);
  dt[4].val = __intg(tm->tm_hour);
  dt[5].val = __intg(tm->tm_min);
  dt[6].val = __intg(tm->tm_sec);

  return(pl_unify(t, dt));
}

int pl_gmtime(term_t t)
{
  time_t now;
  struct tm *tm;

  now = time(0);
  tm  = gmtime(&now);

  return(_pl_time(t, tm));
}

int pl_localtime(term_t t)
{
  time_t now;
  struct tm *tm;

  now = time(0);
  tm  = localtime(&now);

  return(_pl_time(t, tm));
}

int pl_sleep(term_t t)
{ int sec;

  if (PL_get_integer(t, &sec))
  { sleep(sec);
    succeed;
  }
  else
  { // FIXME : error msg
    fail;
  }
}
