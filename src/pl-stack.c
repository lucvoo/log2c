/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#include "Prolog.h"
#include "pl-stack.h"

// #define  DEBUG_STACKS

typedef struct { void *base;
                 void *top;
                 int   size;
                 int   incr;
                 int   max;
                 const char *name;
               } Stack;

typedef void (*sighandler_t)(int);

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/mman.h>
#include <signal.h>


#define	STACK_DEF(NBR,BASE,INCR,MAX,NAME) NBR,
typedef enum { 
#include "pl-stack.h"
             } stack_id;
#undef  STACK_DEF

#define	STACK_DEF(NBR,BASE,INCR,MAX,NAME) { (void *) BASE, 0, 0, INCR, MAX, NAME },
static
Stack stacks[] = { 
#include "pl-stack.h"
                 };
#undef  STACK_DEF

#define NBR_STKS	(sizeof(stacks)/sizeof(stacks[0]))


static
void expand_stack(Stack *s)
{ int new_size;

  new_size=s->size+s->incr;

  if (new_size>s->max)
    { fprintf(stderr,"Out of %s stack\n",s->name);
#ifdef	DEBUG_STACKS
      fprintf(stderr,"SP=%p\t HP=%p\t SHP=%p\t TP=%p\n",SP,HP,SHP,TP);
      fprintf(stderr,"base=%p\t top=%p\t size=%x\t max=%x\n",s->base,s->top,s->size,s->max);
#endif
      exit(2);
//      signal(SIGSEGV, SIG_DFL);
      return;
    }

  if (mmap(s->top, s->incr,
           PROT_READ|PROT_WRITE,
           MAP_ANON|MAP_PRIVATE|MAP_FIXED,
           -1, 0) != s->top)
    { fprintf(stderr,"Can't expand %s stack\n",s->name);
#ifdef	DEBUG_STACKS
      fprintf(stderr,"SP=%p\t HP=%p\t TP=%p\n",SP,HP,TP);
      fprintf(stderr,"base=%p\t top=%p\t size=%x\t max=%x\n",s->base,s->top,s->size,s->max);
#endif
      perror("mmap error");
      exit(3);
      return;
    }
 
  s->size+=s->incr;
  s->top +=s->incr;

#ifdef	DEBUG_STACKS
  fprintf(stderr, __FUNCTION__ ":%d\n", __LINE__);
  fprintf(stderr,"base=%p\t top=%p\t size=%x\t max=%x\t name=%s\n",s->base,s->top,s->size,s->max,s->name);
  fprintf(stderr, "\n");
#endif

  return;
}

// FIXME (4<<10) ?
static
Stack *which_stack(void)
{
#ifdef DEBUG_STACKS
  fprintf(stderr, __FUNCTION__ ":%d\n", __LINE__);
  fprintf(stderr,"SP =%p\t top[local]=%p\n", SP, stacks[LOCAL_STK].top);
  fprintf(stderr,"FP =%p\t top[local]=%p\n", FP, stacks[LOCAL_STK].top);
  fprintf(stderr,"HP =%p\t top[ heap]=%p\n", HP, stacks[ HEAP_STK].top);
  fprintf(stderr,"TP =%p\t top[trail]=%p\n", TP, stacks[TRAIL_STK].top);
  fprintf(stderr,"SHP=%p\t top[sheap]=%p\n",SHP, stacks[SHEAP_STK].top);
#endif
  if ((void *) SP>=stacks[LOCAL_STK].top-(4<<10))
    return(stacks+LOCAL_STK);
  if ((void *) FP>=stacks[LOCAL_STK].top-(4<<10))
    return(stacks+LOCAL_STK);
  if ((void *) HP>=stacks[ HEAP_STK].top-(4<<10))
    return(stacks+ HEAP_STK);
  if ((void *) TP>=stacks[TRAIL_STK].top-(4<<10))
    return(stacks+TRAIL_STK);
  if ((void *)SHP>=stacks[SHEAP_STK].top-(4<<10))
    return(stacks+SHEAP_STK);

  return(0);
}

static
void segv_handler(int sig)
{ Stack *s;

   s=which_stack();

   if (!s)
     { signal(SIGSEGV,SIG_DFL);
#ifdef	DEBUG_STACKS
       { int i;
         fprintf(stderr, __FUNCTION__ ":%d\n", __LINE__);
         fprintf(stderr,"SP=%p\t FP=%p\t HP=%p\t TP=%p\t SHP=%p\n",
         		SP,FP, HP,TP,SHP);
         for (i=0;i<NBR_STKS;i++)
            { s=stacks+i;
              fprintf(stderr,"base=%p\t top=%p\t size=%x\t max=%x\t name=%s\n",s->base,s->top,s->size,s->max,s->name);
            }
         fprintf(stderr, "\n");
       }
#endif
     }
   else
     { expand_stack(s);
       signal(SIGSEGV, segv_handler);
     }
}

// #####################################################################
  
#include "pl-init.h"

void PL_init_stacks(void)
{ int i;

  for (i=0;i<NBR_STKS;i++)
     { stacks[i].top=stacks[i].base;
       // if (munmap(stacks[1].base,stacks[1].max))
       //   { perror("munmap error :"); exit(4); }
       expand_stack(stacks+i);
     }

     STK=stacks[LOCAL_STK].base; SP=STK;
   H_STK=stacks[ HEAP_STK].base; HP=H_STK;
  SH_STK=stacks[SHEAP_STK].base; SHP=SH_STK;
  TR_STK=stacks[TRAIL_STK].base; TP=TR_STK;

  signal(SIGSEGV, segv_handler);
}

/*******************************************************/

int PL_get_stack_stat(struct pl_stack_stat *ss)
{ void *p;

  p = (SP > FP) ? SP : FP;
  ss->used.local = p - stacks[LOCAL_STK].base;
  ss->free.local = stacks[LOCAL_STK].top - p;

  p = HP;
  ss->used.heap  = p - stacks[ HEAP_STK].base;
  ss->free.heap  = stacks[ HEAP_STK].top - p;

  p = TP;
  ss->used.trail = p - stacks[TRAIL_STK].base;
  ss->free.trail = stacks[TRAIL_STK].top - p;

  p = SHP;
  ss->used.sheap = p - stacks[SHEAP_STK].base;
  ss->free.sheap = stacks[SHEAP_STK].top - p;

  return(0);
}

