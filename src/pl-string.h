/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef	PL_STRING_H_
#define PL_STRING_H_

#include <string.h>

#ifdef __linux__
// inline asm for linux
// These ones are from <asm/string.h>
// Copyright (C) 1991, 1992 Linus Torvalds

extern inline char * strcpy(char * dest,const char *src)
{
void *HP_=HP, *FP_=FP;
HP=(void *) dest;
FP=(void *) src;
__asm__ __volatile__(
        "cld\n"
        "1:\tlodsb\n\t"
        "stosb\n\t"
        "testb %%al,%%al\n\t"
        "jne 1b"
        : /* no output */
        : :"di","si","ax","memory");
HP=HP_; FP=FP_;
return dest;
}

// like strcpy but use %edi as dest
// but leave %edi on the end of the copied aera
extern inline void strcpy_heap(const char *src);
extern inline void strcpy_heap(const char *src)
{
void *FP_=FP;
FP=(void *) src;
__asm__ __volatile__(
        "cld\n"
        "1:\tlodsb\n\t"
        "stosb\n\t"
        "testb %%al,%%al\n\t"
        "jne 1b\n\t"
        "decl %%edi"
        : /* no output */
        : :"di","si","ax","memory");
FP=FP_;
return;
}

extern inline char * strcat(char * dest,const char * src)
{
void *HP_=HP, *FP_=FP;
HP=(void *) dest;
FP=(void *) src;
__asm__ __volatile__(
        "cld\n\t"
        "repne\n\t"
        "scasb\n\t"
        "decl %1\n"
        "1:\tlodsb\n\t"
        "stosb\n\t"
        "testb %%al,%%al\n\t"
        "jne 1b"
        : /* no output */
        :"a" (0),"c" (0xffffffff):"si","di","ax","cx");
HP=HP_; FP=FP_;
return dest;
}

extern inline int strcmp(const char * cs,const char * ct)
{
void *HP_=HP, *FP_=FP;
register int __res;
HP=(void *) ct;
FP=(void *) cs;
__asm__ __volatile__(
        "cld\n"
        "1:\tlodsb\n\t"
        "scasb\n\t"
        "jne 2f\n\t"
        "testb %%al,%%al\n\t"
        "jne 1b\n\t"
        "xorl %%eax,%%eax\n\t"
        "jmp 3f\n"
        "2:\tsbbl %%eax,%%eax\n\t"
        "orb $1,%%eax\n"
        "3:"
        :"=a" (__res)
        : : "si", "di");
HP=HP_; FP=FP_;
return __res;
}

extern inline size_t strlen(const char * s)
{
void *HP_=HP;
register int __res;
HP=(void *) s;
__asm__ __volatile__(
        "cld\n\t"
        "repne\n\t"
        "scasb\n\t"
        "notl %0\n\t"
        "decl %0"
        :"=c" (__res):"a" (0),"0" (0xffffffff):"di");
HP=HP_;
return __res;
}

extern inline void * __memcpy(void * to, const void * from, size_t n);

extern inline void * __memcpy(void * to, const void * from, size_t n)
{
void *HP_=HP, *FP_=FP;
HP=(void *) to;
FP=(void *) from;
__asm__ __volatile__(
        "cld\n\t"
        "rep ; movsl\n\t"
        "testb $2,%b1\n\t"
        "je 1f\n\t"
        "movsw\n"
        "1:\ttestb $1,%b1\n\t"
        "je 2f\n\t"
        "movsb\n"
        "2:"
        : /* no output */
        :"c" (n/4), "q" (n)
        :"di","si","cx","memory");
HP=HP_; FP=FP_;
return (to);
}

#endif


INLINE_DECL
int streq(const char *s, const char *d)
{ return(strcmp(s,d)==0); }

INLINE_DECL
int streq_2(const char *src1, const char *src2, register const char *d)
{ register const char *s;
  register char c;

  s=src1;
  while ((c=*s))
    if (c!=*d) goto FAIL;
    else { s++; d++; }

  s=src2;
  while ((c=*s))
    if (c!=*d) goto FAIL;
    else { s++; d++; }

  return(*d=='\0');

  FAIL: 
  return(0);
}

INLINE_DECL
size_t str__len(const char *s)
{ const char *p=s;

  while (*p) p++;

  return(p-s);
}

INLINE_DECL
char *new_str(const char *s1)
{ char *a;
  char *s=(char *)SHP;

  while(*s1) *s++=*s1++;
  *s++='\0';
  
  a=(char *)SHP;
  SHP=Align(s);
  return(a);
}

INLINE_DECL
const char *new_str_2(const char *s1, const char *s2)
{ register char *d=(char *)SHP;
  register const char *s;

  s=s1;
  while(*s) *d++=*s++;
  s=s2;
  while(*s) *d++=*s++;
  *d++='\0';
  
  s=(char *)SHP;
  SHP=Align(d);
  return(s);
}

INLINE_DECL
int isPrefix(const char *s, const char *q)	/* s is prefix of q */
{ while(*s && *s == *q)
    s++, q++;

  return *s == '\0';
}

#endif	// PL_STRING_H_
