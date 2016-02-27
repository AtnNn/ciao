/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

/* First, if we are a BSD system, a line is printed:
 *      #define ENG_BSD 1
 * Then, we test for endian-ness and print a line
 *      #define BIGENDIAN <bool>
 * Then, we test whether the top 4 bits matter in memory accesses.
 * We also test what part of the address space that malloc() places
 * its objects in: if the top 4 bits matter, then a line
 *	#define MallocBase <base>
 * is printed, <base> being the top 4 bits for malloc() pointers.
 * CIAO assumes that those 4 bits are always the same.
 * (MCL) A line is the printed to store the minimum amount of memory which 
 * should be allocated to make the previous true:
 *      #define MIN_MEM_ALLOC <minimum>
 * Then, two lines
 *      #define ENG_FLT_SIGNIF <int>
 *      #define ENG_FLT_ROUND <float>
 * denoting respectively #significant digits in a float and a number to
 * be used for rounding purposes when floats are printed.
 * Then, a line is printed telling whether the system is being built with
 * native code support.
 */

#if defined(__STDC__)
#define VOLATILE volatile
#define PROTO(argl) argl
#else
#define VOLATILE
#define PROTO(ignore) ()
#endif

#if defined(__svr4__) || defined(Solaris) || defined(DARWIN)
# include <stdlib.h>                                           /* malloc() */
#else                                                             /* SunOS */
# include <sys/types.h>
# include <malloc.h>
#endif


#include <stdio.h>
#include <unistd.h>
#include <setjmp.h>
#include <signal.h>
#include <string.h>
#include <math.h>

/* Import basic engine definitions */
#include "compat.h"
#include "termdefs.h"                          /* because of TAGGED (MCL) */
#include "own_malloc_defs.h"

/* Computing the accuracy of floats - Changed so that display(5.347) = 5.347 */

// static ENG_INT base = 2; 

/*
  safe_addition is intended to force A and B to be stored prior to doing the
  addition of A and B , for use in situations where optimizers might hold
  one of these in a register.

  There is a problem with gcc (3.1, at least): when using -O3, which turns
  inlining on, the number of digits in the (decimal) mantissa returned is
  20, due to the inlining of the safe_addition function, and the further
  optimization this brings about.  In order to avoid this in general, I have
  put the volatile keyword which tells the optimizer not to throw away
  references to the ret_val variable (since it may be read/written to by
  other threads :-), even if safe_addition is inlined.
*/

ENG_FLT safe_addition(volatile ENG_FLT *a, volatile ENG_FLT *b) {
  volatile ENG_FLT ret_val;
  ret_val = *a + *b;
  return ret_val;
} 

/* Computing the accuracy of floats - Changed so that display(5.347) = 5.347 */

void find_fp_bits(ENG_INT *t)
{
  volatile static ENG_FLT one = 1.0;
  static ENG_INT base = 2; 

  /* 'base' was originally found out from the implementation of the FPU/FP
     routines; it is tipically 2 in mos FP implementations.  I suppose,
     then, that we can choose whatever base is appropriate for us and use
     the loop below to determine the number of significant digits in
     (pseudo-) base 10. */
    
  volatile ENG_FLT a, c, tmp1;
  volatile ENG_INT lt;

  lt = 0;
  a = c = one;
  while (c == one) {
    ++lt;
    a *= base;
    c = safe_addition(&a, &one);
    tmp1 = -a;
    c = safe_addition(&c, &tmp1);
  }
  *t = lt;
} 

void configure__fpbits() {
  ENG_INT bits;
  double f;    
  int i, j;

  find_fp_bits(&bits);

  i = (bits*0.301029995663981); /* #significant digits, bits*log_10(2) */

  f = 0.5e-9;			/* rounding factor if above 18 */
  for (j=18; j>i; j--)
    f*=10.0;

  printf("#define ENG_FLT_SIGNIF %d\n", i);
  printf("#define ENG_FLT_ROUND %.*g\n\n", i, f);
}

/* ------------------------------------------------------------------------- */
/* configure__endianness: Obtain system endianness
 * 
 * BIGENDIAN: 1 if the system is big-endian, 0 if the system is little-endian
 */

void configure__endianness() {
  union {
    unsigned short as_short[2];
    TAGGED as_tagged;
  } u;
  u.as_tagged = 1;
  printf("#define BIGENDIAN %d\n", (int)u.as_short[1]);
}

/* ------------------------------------------------------------------------- */
/* configure__alloc: Memory management configuration
 *
 * MallocBase: 0xM0000000 where M are the top 4 bits for pointers
 *   returned by malloc()
 * MIN_MEM_ALLOC: minimum amount of memory that makes malloc return
 *   pointers in that region
 *
 * Some systems (namely, LINUX) allocate memory in different parts of
 * the memory depending on how much we ask.  The result is that blocks
 * can be scattered so that the "unmutable four top bits" assumption
 * is broken (i.e., the mapped memory can't be fit into the pointer
 * part of a tagged word) and MallocBase is not useful at all.  We try
 * to find out dynamically if this ever happens, and at which point.
 * This will serve to calculate the minimum amount of memory to be
 * requested at a time from the system, and a (hopefully) correct
 * MallocBase.
 *  
 * If we, anyway, choose to build a system without snooping for a
 * good MallocBase, just use the first pointer the system returns.
 */

/* 
    Before switching to ld linker scripts, things to know (statically):

    * Do we have a working mmap() [which makes it necessary to use
    own_malloc()]?  If so, does it have the ANONYMOUS flag?  If it doesn't,
    does "/dev/zero" work?  We assume we can know this before compiling, and
    find out where does memory start.

    * Otherwise, do we need to grab a minimum amount of memory from malloc
    to make the region stable?

    * If we don't, then just take the first points malloc() gives us.
*/

#if defined(HAS_MMAP)
# define USE_OWN_MALLOC
# include <unistd.h>
# include <sys/mman.h>
#endif

#define MIN_MEM_BLOCK_CHARS 16384

#define BIGTAGMASK 0xf0000000
#define ALIGN sizeof(TAGGED)                        /* Minimum block size */
#define CHARS_TO_TW(Chars) ((Chars)%ALIGN==0 ? (Chars)/ALIGN : (Chars)/ALIGN+1)
#define MIN_MEM_BLOCK (unsigned int)(CHARS_TO_TW(MIN_MEM_BLOCK_CHARS))
#define AddressableSpace 0x10000000



// ---------------------------------------------------------------------------

void write_mem_settings(TAGGED *base,
                        int space,
                        int min_block) {
//  if (use_mmap)       printf("#define USE_MMAP 1\n");
//  if (use_mmap &&
//      dev_zero)       printf("#define USE_DEV_ZERO 1\n");
  printf("#define MallocBase 0x%lx\n", (unsigned long int)base);
  if (space != 0)     printf("#define AddressableSpace 0x%x\n", space);
  if (min_block != 0) {
    printf("#if !defined(USE_OWN_MALLOC)\n#define USE_OWN_MALLOC\n#endif\n");
    printf("#define MIN_MEM_ALLOC 0x%x\n", min_block);
  }
}


// ---------------------------------------------------------------------------

#if defined(HAS_MMAP)

#include "mmap_defs.h" 

#define N_TRY_ADDR 9
unsigned int try_addr[N_TRY_ADDR] = { 
    0x60000000, 0x70000000, 0x80000000, 0x40000000, 0x90000000,
    0x30000000, 0xA0000000, 0x20000000, 0xB0000000};

TAGGED * configure_mmap(void)
{
  TAGGED * pointer;
  int i = 0;
  int fd_mmap_file = -1;

  // Chunks of code identical to those in own_malloc_linear.c - keep them in sync!
#if !defined(ANONYMOUS_MMAP)
  char *mmap_file = MMAP_FILE;

  if ((fd_mmap_file = open(MMAP_FILE, O_RDWR|O_CREAT)) < 0) {
    fprintf(stderr, "\n\n**** Error opening map file in configure.c ****\n\n");
    exit(1);
  }
#endif

  while (i <  N_TRY_ADDR) {
    pointer = mmap((char *)try_addr[i], 
                   AddressableSpace, 
                   PROT_READ|PROT_WRITE,
                   MMAP_FLAGS,
                   fd_mmap_file, 
                   0);
    if (pointer == (TAGGED *)try_addr[i]) 
      break;
    else
      i = i + 1;
  }
    munmap(pointer, AddressableSpace);    // Give back memory
#if !defined(ANONYMOUS_MMAP)
    close(fd_mmap_file);
#endif
    if (i < N_TRY_ADDR) {
      write_mem_settings(pointer, AddressableSpace, AddressableSpace);
      return pointer;
    } else {
      fprintf(stderr, "After trying mmap: no success!");
    return NULL;
    }
}
#endif

// ---------------------------------------------------------------------------

// USE_OWN_MALLOC is always defined whenever HAS_MMAP is, but it may be defined
// in cases where HAS_MMAP is not.

#if defined(USE_OWN_MALLOC) /* malloc() to allocate large blocks of memory */ 
void try_malloc_chunks(void) {
  TAGGED malloc_base = (TAGGED)0;
  int *chunk;
  int min_mem_alloc = getpagesize();
  int size;

  size = ALIGN;
  while (size < AddressableSpace) {             /* Obtain turn point */ 
    chunk = (int *)malloc(size);
    if (chunk == NULL) { /* BIGTAGMASK is never 0, use MIN_MEM_BLOCK */
      malloc_base = (TAGGED)0;
      min_mem_alloc = MIN_MEM_BLOCK;
      break;
    } else {
      if (((TAGGED)chunk & BIGTAGMASK) == 0) {// not yet non-zero, continue 
        size *= 2;
        free(chunk);
      } else { /* Use that one, assume that there will be no more changes in
                  the upper bits */
        malloc_base = (TAGGED)chunk & BIGTAGMASK;
        free(chunk);
	int tagged_lots = CHARS_TO_TW(size);
	min_mem_alloc = (tagged_lots > MIN_MEM_BLOCK ?
			 tagged_lots : MIN_MEM_BLOCK);
        break;
      }
    }
  }
  write_mem_settings((TAGGED *)malloc_base, AddressableSpace, min_mem_alloc);
}
#endif

// ---------------------------------------------------------------------------

#if !defined(USE_OWN_MALLOC)
void try_malloc(void){
  TAGGED malloc_base;
  unsigned char *chunk;

  /* Trust that the malloc implementation gives pointers in the
     0xMmmmmmmm region, where 0xM0000000 is the base */

  chunk = malloc(MIN_MEM_BLOCK_CHARS);
  malloc_base = (TAGGED)chunk & BIGTAGMASK;
  free(chunk);
  write_mem_settings((TAGGED *)malloc_base, 0, 0);
}
#endif

// ---------------------------------------------------------------------------



void configure__alloc() {
#if defined(USE_OWN_MALLOC)
# if defined(HAS_MMAP)
  TAGGED * mmap_base;

  mmap_base = configure_mmap();  // Writes #defines if configuration successful

  if (mmap_base == NULL)
    try_malloc_chunks();
# else     // No mmap() available, or not working
  try_malloc_chunks();
# endif
#else  // !USE_OWN_MALLOC : Last desperate resort
  try_malloc();
#endif
}

/* ------------------------------------------------------------------------- */

/* SunOs does not include strsep  */
#if defined(SunOS4) || defined(Solaris) || defined(IRIX)
/*      $NetBSD: strsep.c,v 1.8 1998/10/13 20:32:09 kleink Exp $        */
/*-
 * Copyright (c) 1990, 1993
 *      The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
 * Get next token from string *stringp, where tokens are possibly-empty
 * strings separated by characters from delim.  
 *
 * Writes NULs into the string at *stringp to end tokens.
 * delim need not remain constant from call to call.
 * On return, *stringp points past the last NUL written (if there might
 * be further tokens), or is NULL (if there are definitely no more tokens).
 *
 * If *stringp is NULL, strsep returns NULL.
 */
char *strsep(stringp, delim)
     char **stringp;
     const char *delim;
{
  char *s;
  const char *spanp;
  int c, sc;
  char *tok;
  
  if ((s = *stringp) == NULL) return (NULL);
  for (tok = s;;) {
    c = *s++;
    spanp = delim;
    do {
      if ((sc = *spanp++) == c) {
        if (c == 0) s = NULL;
        else s[-1] = 0;
        *stringp = s;
        return (tok);
      }
    } while (sc != 0);
  }
}
#endif

void generate_defines(char *cflags) {
  char *Dpointer;
  char *definition;
  char *macroname = NULL;
  char *definition_value;

  Dpointer = cflags;
  while (Dpointer && (Dpointer = strstr(Dpointer, "-D"))) {
    Dpointer += 2;
    if ((definition = strsep(&Dpointer, " "))) {
      definition_value = definition;
      macroname = strsep(&definition_value, "=");
    }
    if (definition_value)
      printf("#if !defined(%s)\n#define %s %s\n#endif\n\n", 
             macroname, macroname, definition_value);
    else
      printf("#if !defined(%s)\n#define %s\n#endif\n\n", 
             macroname, macroname);
  }
}

/* ------------------------------------------------------------------------- */
/* Call all configuration parts */

int main(int argc, char **argv) {
  if (argc > 0) generate_defines(argv[1]);
  configure__endianness();
  configure__alloc();
  configure__fpbits();
  return 0;
}

