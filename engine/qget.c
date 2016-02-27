/* Copyright (C) 1996,1997,1998, UPM-CLIP */
#include <stdlib.h>

#include "threads.h"
#include "datadefs.h"
#include "support.h"


/* declarations for global functions accessed here */

#include "qget_defs.h"
#include "qread_defs.h"
#include "bignum_defs.h"
#include "main_defs.h"

/* local declarations */



/* Shared? Might be; then, only one thread may read in ql's at a time.
   Since it is not a very common situation, we might as well lock ql
   reading.  In any case, access to program area should be locked, and so we
   do not loose anything. */

/* I am not really sure about the length of the strings which can appear in
   a .po file.  If they are longer than STATICMAXATOM, the emulator will for
   sure break.  I will add DEBUG options to check this. */

#define WORKSTRINGLEN (STATICMAXATOM)

char workstring[WORKSTRINGLEN]; 

int getshort(f)
     FILE *f;
{
  REGISTER char *ws;
#if defined(DEBUG)
  REGISTER int used_length = 0;
#endif
  
  for(ws = workstring; (*ws++ = GETC(f));)
#if defined(DEBUG)
    if (++used_length > WORKSTRINGLEN)
      SERIOUS_FAULT("workstring length exceeded in getshort()")
#else
    ;
#endif

  return atoi(workstring);
}

ENG_INT getlong(f)
     FILE *f;
{
  REGISTER char *ws;
#if defined(DEBUG)
  REGISTER int used_length = 0;
#endif
  
  for(ws = workstring; (*ws++ = GETC(f));)
#if defined(DEBUG)
    if (++used_length > WORKSTRINGLEN)
      SERIOUS_FAULT("workstring length exceeded in getlong()")
#else
    ;
#endif

  return atol(workstring);
}

TAGGED getlarge(Arg,f)
     Argdecl;
     FILE *f;
{
  REGISTER int i;
  REGISTER char *ws;

  for (i=0; TRUE;)  {
    ws = Atom_Buffer;
    while (i<Atom_Buffer_Length)
      if (!(ws[i++] = GETC(f)))
        goto out;
    Atom_Buffer = (char *)checkrealloc((TAGGED *)ws,
                                       i, Atom_Buffer_Length<<=1);
  }
 out:
  if (bn_from_string(ws,w->global_top,Heap_End))
    {
      SERIOUS_FAULT("$qload: miscalculated heap usage");
    }
  else
    {
      TAGGED *h = w->global_top;
      int ar = LargeArity(h[0]);
      
      if (ar==2 && IntIsSmall((int)h[1]))
	return MakeSmall(h[1]);
      else
	{
	  w->global_top += ar+1;
	  h[ar] = h[0];
	  return Tag(STR,h);
	}
    }
}

ENG_FLT getdouble(f)
     FILE *f;
{
  REGISTER char *ws;
#if defined(DEBUG)
  REGISTER int used_length = 0;
#endif
  
  for(ws = workstring; (*ws++ = GETC(f));)
#if defined(DEBUG)
    if (++used_length > WORKSTRINGLEN)
      SERIOUS_FAULT("workstring length exceeded in getdouble()")
#else
    ;
#endif

  return atof(workstring);
}

char *getstring(f)
     FILE *f;
{
  REGISTER char *ws;
#if defined(DEBUG)
  REGISTER int used_length = 0;
#endif
  
  for(ws = workstring; (*ws++ = GETC(f));)
#if defined(DEBUG)
    if (++used_length > WORKSTRINGLEN)
      SERIOUS_FAULT("workstring length exceeded in getstring()")
#else
    ;
#endif

  return workstring;
}


void getbytecode(Arg,f,insn_p,length)
     Argdecl;
     FILE *f;
     INSN *insn_p;
     int length;
{
  REGISTER char c;
  /*extern ENG_INT atol PROTO((char *str)); */
  /*extern ENG_FLT atof PROTO((char *str));*/

  while ((c=GETC(f))) {
    switch (c) {
    case 'G': {
      REGISTER int i;
      REGISTER char *ws;
      BOOL floatp = FALSE;
      TAGGED *wp = (TAGGED *)insn_p;
      
      for (i=0; TRUE;) {
        ws = Atom_Buffer;
        while (i<Atom_Buffer_Length)
          if (!(ws[i++] = c = GETC(f)))
            goto out;
          else if (c == '.')
            floatp = TRUE;
        Atom_Buffer = (char *)checkrealloc((TAGGED *)ws,
                                           i, Atom_Buffer_Length<<=1);
      } 
    out:
      if (floatp) {
        ENG_FLT f = atof(ws);
        TAGGED *fp = (TAGGED *)(&f);
        
        *wp++ = MakeFunctorFloat;
        *wp++ = fp[0];
        *wp++ = fp[1];
        insn_p += 6;
      } else {
        bn_from_string(ws,(Bignum *)insn_p,(Bignum *)((char *)insn_p+length));
        insn_p += LargeArity(*wp)<<1;
      }
      break;
    }
      
    case '+': {
      REGISTER char *ws = workstring;
#if defined(DEBUG)
      REGISTER int used_length = 0;
#endif
      
      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if (++used_length > WORKSTRINGLEN)
          SERIOUS_FAULT("workstring length exceeded in getbytecode()")
#else
        ;
#endif

      *(ENG_INT *)insn_p = atol(workstring);
      insn_p += BPL;
      break;
    }
      
    case 'C': {
      REGISTER char *ws = workstring;
#if defined(DEBUG)
      REGISTER int used_length = 0;
#endif
      
      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if (++used_length > WORKSTRINGLEN)
          SERIOUS_FAULT("workstring length exceeded in getbytecode()")
#else
        ;
#endif

      *(CInfo *)insn_p = builtintab[atoi(workstring)];
      insn_p += BPTP;
      break;
    }
      
    case 'F': {
      REGISTER char *ws = workstring;
#if defined(DEBUG)
      REGISTER int used_length = 0;
#endif
      
      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if (++used_length > WORKSTRINGLEN)
          SERIOUS_FAULT("workstring length exceeded in getbytecode()")
#else
        ;
#endif

      /* Was:
       *insn_p++ = (INSN)builtintab[atoi(workstring)]);
       ant it issued a mesage since INSN is short int and *builtintab
       is a pointer to a function */
      *insn_p++ = (INSN)((unsigned long int)builtintab[atoi(workstring)]);
      break;
    }
    case 'D': {
      REGISTER char *ws = workstring;
#if defined(DEBUG)
      REGISTER int used_length = 0;
#endif
      
      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if (++used_length > WORKSTRINGLEN)
          SERIOUS_FAULT("workstring length exceeded in getbytecode()")
#else
        ;
#endif

      *(CInfo *)insn_p = builtintab[atoi(workstring)];

      ws = workstring;
#if defined(DEBUG)
      used_length = 0;
#endif

      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if (++used_length > WORKSTRINGLEN)
          SERIOUS_FAULT("workstring length exceeded in getbytecode()")
#else
        ;
#endif

      *(long *)insn_p += atol(workstring);
      insn_p += BPTP;
      break;
    }
      
    case 'E': {
      REGISTER char *ws = workstring;
#if defined(DEBUG)
      REGISTER int used_length = 0;
#endif
      
      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if (++used_length > WORKSTRINGLEN)
          SERIOUS_FAULT("workstring length exceeded in getbytecode()")
#else
        ;
#endif

      *(unsigned long *)insn_p =
        (unsigned long)builtintab[atoi(workstring)] 
          - (unsigned long)insn_p;
      insn_p += BPTP;
      break;
    }
      
    default: {
      REGISTER char *ws = workstring;
#if defined(DEBUG)
      REGISTER int used_length = 0;
#endif
      
      *ws++ = c;
      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if (++used_length > WORKSTRINGLEN)
          SERIOUS_FAULT("workstring length exceeded in getbytecode()")
#else
        ;
#endif

      *insn_p++ = atoi(workstring);
    }
    }
  }
}

