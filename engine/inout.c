/* Copyright (C) 1996,1997,1998, UPM-CLIP */

#include <stdio.h>
#include <errno.h>
#include <unistd.h>

void perror() /*, ENG_perror()*/;
extern int errno;

#include "compat.h"
#include "datadefs.h"
#include "support.h"
#include "ciao.h"
#include "threads.h"
#include "task_areas.h"

/* declarations for global functions accessed here */

#include "term_support_defs.h"
#include "inout_defs.h"
#include "streams_defs.h"
#include "tasks_defs.h"
#include "main_defs.h"

/* local declarations */

static void writechar(int ch, register int i, register struct stream_node *s);
static int readchar(register struct stream_node *s, int type, struct definition *pred_address);
static void display_term(Argdecl, TAGGED term, struct stream_node *stream, BOOL quoted);
BOOL code_class(Argdecl);
BOOL peek(Argdecl);
BOOL peek2(Argdecl);


BOOL code_class(Arg)
     Argdecl;
{
  int i;
  
  DEREF(X(0),X(0));
  if (!TagIsSmall(X(0)) || (i = GetSmall(X(0))) & ~255) /* Not a byte */
    ERROR_IN_ARG(X(0),1,BYTE)

  return cunify(Arg,X(1),MakeSmall(symbolchar[i]));
}

#define INC_COUNTS(ch,stream) \
{ \
    if (ch == (int)'\n')  \
      stream->last_nl_pos = stream->char_count += 1, stream->nl_count++; \
    else \
      stream->char_count++; \
}

static void writechar(ch,i,s)
     int ch;
     REGISTER int i;
     REGISTER struct stream_node *s;
{
  REGISTER FILE *f = s->streamfile;
  
  if (s->isatty)
    s = root_stream_ptr;
  while (--i >= 0){
    if (s->streammode != 's')                              /* Not a socket */
      putc(ch, f);
    else {
      char p;
      p = (char)ch;
      write(GetInteger(s->label), &p, (size_t)1);
    }
    INC_COUNTS(ch,s);
  }
}


int (*ENG_read_hook)() = NULL;

#define SAVE_XREGS(where, how_many) \
{ \
    int i; \
    struct worker *w = get_my_worker(); \
    if (!w) SERIOUS_FAULT("Could not get worker in SAVE_XREGS"); \
    for(i = 0; i < how_many; i++) \
      (where)[i] = X(i); \
}

#define RESTORE_XREGS(where, how_many) \
{ \
    int i; \
    struct worker *w = get_my_worker(); \
    if (!w) SERIOUS_FAULT("Could not get worker in RESTORE_XREGS"); \
    for(i = 0; i < how_many; i++) \
      X(i) = (where)[i];  \
}

#define REGS_TO_SAVE 32                            /* How many, actually? */

#define PEEK -100
#define GET   -10
#define GET1   -1

#define EXITCOND(op,i) \
  (op<GET1 || i==EOF || (op==GET1 && symbolchar[i]>0) || op==i)

/* Returns -2 when attempting to read past end of file //) */
static int readchar(s,op_type,pred_address)
     struct stream_node *s;
     int op_type;  /* PEEK = -100, GET = -10, GET1 = -1, SKIP >= 0 */
     struct definition *pred_address;
{
  FILE *f = s->streamfile;
  int i;
  unsigned char ch;
  TAGGED saved_regs[REGS_TO_SAVE];

  if (s->isatty) {
    int_address = pred_address;
    while (TRUE) {
      if (root_stream_ptr->char_count==root_stream_ptr->last_nl_pos)
        print_string(stream_user_output,GetString(current_prompt)),
        fflush(stdout);
                                                     /* MCL: ENG_read_hook */
      if (ENG_read_hook != NULL){
        SAVE_XREGS(saved_regs, REGS_TO_SAVE);
        do               /* Call ENG_read_hook until input available at fd */
          i = (*ENG_read_hook)(fileno(f));
        while (i == 0);
        RESTORE_XREGS(saved_regs, REGS_TO_SAVE);
      }

      if (s->pending_char >= -1) { /* There is a char returned by peek */
        i = s->pending_char;
        s->pending_char = -100;
      } else
        i = getc(f);

      if (op_type==PEEK)
        s->pending_char = i;
      else
        INC_COUNTS(i,root_stream_ptr);

      if (i==EOF) clearerr(f);

      if (EXITCOND(op_type,i)) {
        int_address = NULL; 
        return i;
      }
    }
  } else if (s->streammode != 's') { /* Not a socket */

    if (feof(f) && s->pending_char == -100) return -2; /* attempt to read past end of stream */
    
    while (TRUE) {
      if (s->pending_char >= -1) { /* There is a char returned by peek */
        i = s->pending_char;
        s->pending_char = -100;
      } else
        i = getc(f);

      if (op_type==PEEK)
        s->pending_char = i;
      else
        INC_COUNTS(i,s);
      
      if (EXITCOND(op_type,i)) return i;
    } 
  } else {                                                  /* A socket */
    int fildes = GetInteger(s->label);
    
    if (s->socket_eof) return -2; /* attempt to read past end of stream */
    
    while (TRUE) {
      if (s->pending_char >= -1) { /* There is a char returned by peek */
        i = s->pending_char;
        s->pending_char = -100;
      } else {
        switch(read(fildes, (void *)&ch, 1)){
        case 0:
          i = EOF;
          break;
        case 1: 
          i = (int)ch;
          break;
        default:
          perror("read() in readchar(): ");
          SERIOUS_FAULT("Aborting");
        }
      }
      
      if (op_type==PEEK) {
        s->pending_char = i;
      } else {
        INC_COUNTS(i,s);
        if (i==EOF) s->socket_eof = TRUE;
      }

      if (EXITCOND(op_type,i)) return i;

    }
  }
}

/*----------------------------------------------------------------*/

BOOL flush_output(Arg)
     Argdecl;
{
  if ((Output_Stream_Ptr->streammode != 's')
      && fflush(Output_Stream_Ptr->streamfile)) {
    ENG_perror("% fflush in flush_output/1");
  }
  return TRUE;
}

/*----------------------------------------------------------------*/

BOOL flush_output1(Arg)
     Argdecl;
{
  int errcode;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s)
    BUILTIN_ERROR(errcode,X(0),1);

  if ((s->streammode != 's') && fflush(s->streamfile)) {
    ENG_perror("% fflush in flush_output/1");
  }
  return TRUE;
}

/*----------------------------------------------------------------*/

BOOL getct(Arg)
     Argdecl;
{
  int i;

  i = readchar(Input_Stream_Ptr,GET,address_getct);

  if (i < -1)
    BUILTIN_ERROR(READ_PAST_EOS_ERROR,atom_nil,0)

  return cunify(Arg,X(0),MakeSmall(i)) 
         && cunify(Arg,X(1),MakeSmall(i == -1 ? -1 : symbolchar[i]));
}


/*----------------------------------------------------------------*/

BOOL getct1(Arg)
     Argdecl;
{
  int i;
  
  i = readchar(Input_Stream_Ptr,GET1,address_getct1); /* skip whitespace */

  if (i < -1)
    BUILTIN_ERROR(READ_PAST_EOS_ERROR,atom_nil,0)

  return cunify(Arg,X(0),MakeSmall(i)) 
         && cunify(Arg,X(1),MakeSmall(i == -1 ? -1 : symbolchar[i]));
}


/*----------------------------------------------------------------*/

BOOL get(Arg)
     Argdecl;
{
  int i;

  i = readchar(Input_Stream_Ptr,GET,address_get);

  if (i < -1)
    BUILTIN_ERROR(READ_PAST_EOS_ERROR,atom_nil,0)

  return cunify(Arg,X(0),MakeSmall(i));
}

/*----------------------------------------------------------------*/

BOOL get2(Arg)
     Argdecl;
{
  int i;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s)
    BUILTIN_ERROR(i,X(0),1);

  i = readchar(s,GET,address_get2);

  if (i < -1)
    BUILTIN_ERROR(READ_PAST_EOS_ERROR,X(0),1)

  return cunify(Arg,X(1),MakeSmall(i));
}

/*----------------------------------------------------------------*/

BOOL get1(Arg)
     Argdecl;
{
  int i;
  
  i = readchar(Input_Stream_Ptr,GET1,address_get1); /* skip whitespace */

  if (i < -1)
    BUILTIN_ERROR(READ_PAST_EOS_ERROR,atom_nil,0)

  return cunify(Arg,X(0),MakeSmall(i));
}


/*----------------------------------------------------------------*/


BOOL get12(Arg)
     Argdecl;
{
  int i;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s)
    BUILTIN_ERROR(i,X(0),1);

  i = readchar(s,GET1,address_get12); /* skip whitespace */

  if (i < -1)
    BUILTIN_ERROR(READ_PAST_EOS_ERROR,X(0),1)

  return cunify(Arg,X(1),MakeSmall(i));
}


/*----------------------------------------------------------------*/

BOOL peek(Arg)
     Argdecl;
{
  int i;

  i = readchar(Input_Stream_Ptr,PEEK,address_peek);

  if (i < -1)
    BUILTIN_ERROR(READ_PAST_EOS_ERROR,atom_nil,0)

  return cunify(Arg,X(0),MakeSmall(i));
}

/*----------------------------------------------------------------*/

BOOL peek2(Arg)
     Argdecl;
{
  int i;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s)
    BUILTIN_ERROR(i,X(0),1);

  i = readchar(s,PEEK,address_peek2);

  if (i < -1)
    BUILTIN_ERROR(READ_PAST_EOS_ERROR,X(0),1)

  return cunify(Arg,X(1),MakeSmall(i));
}

/*----------------------------------------------------------------*/

BOOL nl(Arg)
     Argdecl;
{
  writechar('\n',1,Output_Stream_Ptr);
  return TRUE;
}


/*----------------------------------------------------------------*/

BOOL nl1(Arg)
     Argdecl;
{
  int errcode;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s)
    BUILTIN_ERROR(errcode,X(0),1);

  writechar('\n',1,s);
  return TRUE;
}

/*----------------------------------------------------------------*/

BOOL put(Arg)
     Argdecl;
{
  int i;

  DEREF(X(0),X(0));
  if (!TagIsSmall(X(0)) || (i = GetSmall(X(0))) & ~255) /* Not a byte */
    ERROR_IN_ARG(X(0),1,BYTE)

  writechar(i,1,Output_Stream_Ptr);
  return TRUE;
}

/*----------------------------------------------------------------*/

BOOL put2(Arg)
     Argdecl;
{
  int i;
  struct stream_node *s;

  s = stream_to_ptr_check(X(0), 'w', &i);
  if (!s)
    BUILTIN_ERROR(i,X(0),1);

  DEREF(X(1),X(1));
  if (!TagIsSmall(X(1)) || (i = GetSmall(X(1))) & ~255) /* Not a byte */
    ERROR_IN_ARG(X(1),2,BYTE)

  writechar(i,1,s);
  return TRUE;
}

/*----------------------------------------------------------------*/
/* output stream always write or append */

BOOL tab(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  if (!IsInteger(X(0)))
    ERROR_IN_ARG(X(0),1,INTEGER)

  writechar(' ',GetInteger(X(0)),Output_Stream_Ptr);
  return TRUE;
}


/*----------------------------------------------------------------*/

BOOL tab2(Arg)
     Argdecl;
{
  int errcode;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s)
    BUILTIN_ERROR(errcode,X(0),1);

  DEREF(X(1),X(1));
  if (!IsInteger(X(1)))
    ERROR_IN_ARG(X(1),2,INTEGER)

  writechar(' ',GetInteger(X(1)),s);
  return TRUE;
}

/*----------------------------------------------------------------*/

BOOL skip(Arg)
     Argdecl;
{
  int i, ch;

  DEREF(X(0),X(0));
  if (!TagIsSmall(X(0)) || (i = GetSmall(X(0))) & ~255) /* Not a byte */
    ERROR_IN_ARG(X(0),1,BYTE)

  for (ch=i+1; ch!=i && ch>=-1;)
    ch = readchar(Input_Stream_Ptr,i,address_skip);

  if (ch < -1)
    BUILTIN_ERROR(READ_PAST_EOS_ERROR,atom_nil,0)

  return TRUE;
}

/*----------------------------------------------------------------*/


BOOL skip2(Arg)
     Argdecl;
{
  int i, ch;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s)
    BUILTIN_ERROR(i,X(0),1);

  DEREF(X(1),X(1));
  if (!TagIsSmall(X(1)) || (i = GetSmall(X(1))) & ~255) /* Not a byte */
    ERROR_IN_ARG(X(1),2,BYTE)

  for (ch=i+1; ch!=i && ch>=-1;)
    ch = readchar(s,i,address_skip2);

  if (ch < -1)
    BUILTIN_ERROR(READ_PAST_EOS_ERROR,X(0),1)

  return TRUE;
}

/*----------------------------------------------------------------*/
/*                       Moved from streams.c (DCG)               */
/*----------------------------------------------------------------*/

/* This is essentially an open-coded fputs().  
   fputs() starts paying off at string lengths above 50 or so.
 */
void print_string(stream,p)
     REGISTER char *p;
     REGISTER struct stream_node *stream;
{
  REGISTER FILE *fileptr = stream->streamfile;
  REGISTER int i;

  if (stream->isatty)
    stream = root_stream_ptr;

  if (stream->streammode != 's')                       /* Is not a socket */
    for (i = *p++; i; i = *p++) {
      putc(i,fileptr);
      INC_COUNTS(i,stream);
    }
  else {
    size_t size = 0;
    char *q = p;

    for (i = *q++; i; i = *q++) {
      INC_COUNTS(i,stream);
      size++;
    }
    write(GetInteger(stream->label), p, size);
  }
}


void print_variable(Arg,stream,term)
     Argdecl;
     TAGGED term;
     struct stream_node *stream;
{
  if (IsStackVar(term))
    term = TagHVA(Heap_End+(TagToSVA(term)-Stack_Start));
  print_string(stream, "_");
  number_to_string(Arg,MakeInteger(Arg,TagToPointer(term)-Heap_Start), 10);
  print_string(stream, Atom_Buffer);
}

void print_number(Arg, stream,term)
     Argdecl;
     struct stream_node *stream;
     TAGGED term;
{
  number_to_string(Arg,term, 10);
  print_string(stream, Atom_Buffer);
}

void print_atom(stream,term)
     TAGGED term;
     struct stream_node *stream;
{
  struct atom *atomptr = TagToAtom(term);
  
  if (!atomptr->has_special)
    print_string(stream, atomptr->name);
  else
    {
      char buf[2*MAXATOM+3];
      REGISTER char *ch = atomptr->name;
      REGISTER char *bp = buf;
      REGISTER int i;
      
      *bp++ = '\'';
      if (atomptr->has_squote)
	while ((i = *ch++))
	  {
	    if (i=='\'' || i=='\\')
	      *bp++ = i;
	    *bp++ = i;
	  }
      else
	while ((i = *ch++))
	  {
	    if (i=='\\')
	      *bp++ = i;
            *bp++ = i;
          }
      *bp++ = '\'';
      *bp++ = 0;
      print_string(stream, buf);
    }
}

/*   --------------------------------------------------------------  */	 

static void display_term(Arg, term, stream, quoted)
     Argdecl;
     TAGGED term;
     struct stream_node *stream;
     BOOL quoted;
{
  REGISTER TAGGED aux;
  int arity,i;

  switch (TagOf(term))
    {
    case LST:
      writechar('[',1,stream);
      DerefCar(aux,term);
      display_term(Arg,aux, stream, quoted);
      DerefCdr(term,term);
      while(TagIsLST(term))
	{
	  writechar(',',1,stream);
	  DerefCar(aux,term);
	  display_term(Arg,aux, stream, quoted);
	  DerefCdr(term,term);
	}
      if(term!=atom_nil)
	{
	  writechar('|',1,stream);
	  display_term(Arg,term, stream, quoted);
	}
      writechar(']',1,stream);
      break;
    case STR:
      if (STRIsLarge(term))
	goto number;
      display_term(Arg,TagToHeadfunctor(term),stream, quoted);
      writechar('(',1,stream);
      arity = Arity(TagToHeadfunctor(term));
      for(i=1; i<=arity; i++)
	{
	  if(i>1) writechar(',',1,stream);
	  DerefArg(aux,term,i);
	  display_term(Arg,aux, stream, quoted);
	}
      writechar(')',1,stream);
      break;
    case UBV:
    case SVA:
    case HVA:
    case CVA:
      {
	print_variable(Arg,stream,term);
	break;
      }
    case ATM:
      if (quoted)
        print_atom(stream,term);
      else
        print_string(stream,TagToAtom(term)->name);
      break;
    case NUM:
    number:
      print_number(Arg,stream,term);
      break;
    }
}

BOOL prolog_display(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  display_term(Arg,X(0),Output_Stream_Ptr, FALSE);
  return TRUE;
}

BOOL prolog_display2(Arg)
     Argdecl;
{
  int errcode;
  struct stream_node *stream;
  
  stream = stream_to_ptr_check(X(0), 'w', &errcode);
  if (stream==NULL)
    BUILTIN_ERROR(errcode,X(0),1);

  DEREF(X(1),X(1));
  display_term(Arg,X(1),stream, FALSE);
  return TRUE;
}

BOOL prolog_displayq(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  display_term(Arg,X(0), Output_Stream_Ptr, TRUE);
  return TRUE;
}

BOOL prolog_displayq2(Arg)
     Argdecl;
{
  int errcode;
  struct stream_node *stream;
  
  stream = stream_to_ptr_check(X(0), 'w', &errcode);
  if (stream==NULL)
    BUILTIN_ERROR(errcode,X(0),1);

  DEREF(X(1),X(1));
  display_term(Arg,X(1), stream, TRUE);
  return TRUE;
}

BOOL prolog_clearerr(Arg)
     Argdecl;
{
  int errcode;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s)
    BUILTIN_ERROR(errcode,X(0),1);
  
  if (s->streammode != 's') clearerr(s->streamfile);

  return TRUE;
}
