/*
 *  io_basic.c
 *
 *  Input/output predicates (see engine(io_basic)).
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

/* TODO: This code should be generic for any stream */

#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>  /* for atoi MCL */
#include <string.h>
#include <strings.h>

#include <ciao/os_signal.h>
#include <ciao/threads.h>
#include <ciao/datadefs.h>
#include <ciao/support.h>
#include <ciao/support_macros.h>
#include <ciao/task_areas.h>
#include <ciao/misc.h>
#include <ciao/interrupt.h>

#include <ciao/term_support.h>
#include <ciao/io_basic.h>
#include <ciao/streams_basic.h>
#include <ciao/tasks.h>
#include <ciao/start.h>
#include <ciao/alloc.h>
#include <ciao/bignum.h>
#include <ciao/stacks.h>

/* Own version of getc() that normalizes EOF (<0) to Ciao's CHAR_EOF (-1) */
#if (EOF == CHAR_EOF)
static inline int c_getc(FILE *f) {
  return getc(f);
}
#else
static inline int c_getc(FILE *f) {
  int i = getc(f);
  return (i < 0 ? CHAR_EOF : i);
}
#endif

static int readchar(stream_node_t *s, int type, definition_t *pred_address);
static void writechar(int ch, stream_node_t *s);
static void writecharn(int ch, int i, stream_node_t *s);
static int readbyte(stream_node_t *s, definition_t *pred_address);
static void writebyte(int ch, stream_node_t *s);

CVOID__PROTO(display_term, tagged_t term, stream_node_t *stream, bool_t quoted);

#define CheckGetCharacterCode(X,C,ArgNo) {                              \
    if (TagIsSmall(X)) {                                                \
      if ((C = GetSmall(X)) & ~255) {					\
        BUILTIN_ERROR(REPRESENTATION_ERROR(CHARACTER_CODE), X, ArgNo);  \
      }									\
    }                                                                   \
    else if (TagIsLarge(X) && !LargeIsFloat(X)) { /* bigint */		\
      BUILTIN_ERROR(REPRESENTATION_ERROR(CHARACTER_CODE), X, ArgNo);    \
    }                                                                   \
    else {                                                              \
      ERROR_IN_ARG(X, ArgNo, INTEGER);                                  \
    }                                                                   \
  }

#define CheckGetByte(X,C,ArgNo)					\
  if (!TagIsSmall((X)) || ((C) = GetSmall((X))) & ~255) {	\
    ERROR_IN_ARG((X), (ArgNo), (TY_BYTE));			\
  }

#define IO_ERROR(Message) {			\
    /* TODO: throw an exception instead */	\
    perror((Message));				\
    SERIOUS_FAULT("Aborting");			\
}

CBOOL__PROTO(code_class)
{
  ERR__FUNCTOR("io_basic:code_class", 2);
  int i;

  DEREF(X(0), X(0));
  CheckGetCharacterCode(X(0),i,1);

  return cunify(Arg,X(1),MakeSmall(symbolchar[i]));
}

static inline void inc_counts(int ch, stream_node_t * stream){
  stream->char_count++;
  if (ch == 0xd) {
    stream->last_nl_pos = stream->char_count;
    stream->nl_count++;
  } else if (ch == 0xa) {
    stream->last_nl_pos = stream->char_count;
    if (stream->previous_char != 0xd)
      stream->nl_count++;
  }
  stream->previous_char = ch;
}

static void writechar(int ch, stream_node_t *s) {
  FILE *f = s->streamfile;
  if (s->isatty) {
    s = root_stream_ptr;
    /* ignore errors on tty */
    putc(ch, f);
  } else if (s->streammode != 's') { /* Not a socket */
    if (putc(ch, f) < 0) {
      IO_ERROR("putc() in writechar()");
    }
  } else {
    char p;
    p = (char)ch;
    if (write(GetInteger(s->label), &p, (size_t)1) < 0) {
      IO_ERROR("write() in writechar()");
    }
  }
  inc_counts(ch,s);
}

static void writecharn(int ch, int i, stream_node_t *s) {
  while (--i >= 0) {
    writechar(ch, s);
  }
}

static void writebyte(int ch, stream_node_t *s) {
  FILE *f = s->streamfile;
  if (s->isatty) {
    s = root_stream_ptr;
    /* ignore errors on tty */
    putc(ch, f);
  } else if (s->streammode != 's') { /* Not a socket */
    if (putc(ch, f) < 0) {
      IO_ERROR("putc() in writebyte()");
    }
  } else {
    char p;
    p = (char)ch;
    if (write(GetInteger(s->label), &p, (size_t)1) < 0) {
      IO_ERROR("write() in writebyte()");
    }
  }
}

#define DELRET -5
#define PEEK   -4
#define GET    -3
#define GET1   -2
#define SKIPLN -1

#define EXITCOND(op,i) \
  ( op<GET1 || i==CHAR_EOF || (op==GET1 && symbolchar[i]>0) || \
    (op==SKIPLN && (i==0xa || i==0xd)) || op==i )

#define GIVEBACKCOND(op,i) \
  (op==PEEK || (op==SKIPLN && i==CHAR_EOF) || (op==DELRET && i!=0xa))

/* Returns CHAR_PAST_EOF when attempting to read past end of file)

   op_type: DELRET, PEEK, GET, GET1, SKIPLN, or >= 0 for SKIP
 */
static int readchar(stream_node_t *s,
		    int op_type,
		    definition_t *pred_address)
{
  FILE *f = s->streamfile;
  int i;

  if (s->isatty) {
    int_address = pred_address;
    while (TRUE) {
      if (root_stream_ptr->char_count==root_stream_ptr->last_nl_pos){
        print_string(stream_user_output,GetString(current_prompt));
          /* fflush(stdout); into print_string() MCL */
      }

      if (s->pending_char == CHAR_VOID) { /* There is no char returned by peek */
	/* ignore errors in tty */
	i = c_getc(f);
      } else {
	i = s->pending_char;
        s->pending_char = CHAR_VOID;
      }
      
      if GIVEBACKCOND(op_type,i)
        s->pending_char = i;
      else
        inc_counts(i,root_stream_ptr);

      if (i==CHAR_EOF) clearerr(f);

      if (EXITCOND(op_type,i)) {
        int_address = NULL; 
        return i;
      }
    }
  } else if (s->streammode != 's') { /* Not a socket */

    if (feof(f) && s->pending_char == CHAR_VOID) {
      return CHAR_PAST_EOF; /* attempt to read past end of stream */
    }
    
    while (TRUE) {
      if (s->pending_char != CHAR_VOID) { /* There is a char returned by peek */
        i = s->pending_char;
        s->pending_char = CHAR_VOID;
      } else {
	i = c_getc(f);
	if (i < 0 && ferror(f)) {
	  IO_ERROR("getc() in readchar()");
	}
      }

      if GIVEBACKCOND(op_type,i)
        s->pending_char = i;
      else
        inc_counts(i,s);
      
      if (EXITCOND(op_type,i)) return i;
    }
  } else {                                                  /* A socket */
    int fildes = GetInteger(s->label);
    
    if (s->socket_eof) return CHAR_PAST_EOF; /* attempt to read past end of stream */
    
    while (TRUE) {
      unsigned char ch;
      if (s->pending_char == CHAR_VOID) { /* There is a char returned by peek */
        switch(read(fildes, (void *)&ch, 1)){
        case 0:
          i = CHAR_EOF;
          break;
        case 1: 
          i = (int)ch;
          break;
        default:
          IO_ERROR("read() in readchar()");
        }
      } else {
	i = s->pending_char;
        s->pending_char = CHAR_VOID;
      }
      
      if GIVEBACKCOND(op_type,i)
        s->pending_char = i;
      else {
        inc_counts(i,s);
        if (i==CHAR_EOF) s->socket_eof = TRUE;
      }

      if (EXITCOND(op_type,i)) return i;

    }
  }
}

static int readbyte(stream_node_t *s,
		    definition_t *pred_address)
{
  FILE *f = s->streamfile;
  int i;

  if (s->isatty) {
    int_address = pred_address;
    if (root_stream_ptr->char_count==root_stream_ptr->last_nl_pos){
      print_string(stream_user_output,GetString(current_prompt));
      /* fflush(stdout); into print_string() MCL */
    }

    /* ignore errors in tty */
    i = c_getc(f);
    if (i==CHAR_EOF) clearerr(f);
    
    int_address = NULL; 
    return i;
    
  } else if (s->streammode != 's') { /* Not a socket */

    if (feof(f)) return CHAR_PAST_EOF; /* attempt to read past end of stream */
    i = c_getc(f);
    if (i < 0 && ferror(f)) {
      IO_ERROR("getc() in readbyte()");
    }
    return i;
    
  } else {                                                  /* A socket */
    unsigned char ch;
    int fildes = GetInteger(s->label);
    
    if (s->socket_eof) return CHAR_PAST_EOF; /* attempt to read past end of stream */
    
    switch(read(fildes, (void *)&ch, 1)){
    case 0:
      i = CHAR_EOF;
      break;
    case 1: 
      i = (int)ch;
      break;
    default:
      IO_ERROR("read() in readbyte()");
    }

    if (i==CHAR_EOF) s->socket_eof = TRUE;

    return i;

  }
}


/*----------------------------------------------------------------*/

CBOOL__PROTO(flush_output)
{
  if ((Output_Stream_Ptr->streammode != 's')
      && fflush(Output_Stream_Ptr->streamfile)) {
    ENG_perror("fflush in flush_output/1");
  }
  return TRUE;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(flush_output1)
{
  ERR__FUNCTOR("streams_basic:flush_output", 1);
  int errcode;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  if ((s->streammode != 's') && fflush(s->streamfile)) {
    ENG_perror("fflush in flush_output/1");
  }
  return TRUE;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(getct)
{
  ERR__FUNCTOR("io_basic:getct", 2);
  int i;

  i = readchar(Input_Stream_Ptr,GET,address_getct);

  if (i == CHAR_PAST_EOF) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
  }

  return cunify(Arg,X(0),MakeSmall(i)) 
         && cunify(Arg,X(1),MakeSmall(i == CHAR_EOF ? -1 : symbolchar[i]));
}


/*----------------------------------------------------------------*/

CBOOL__PROTO(getct1)
{
  ERR__FUNCTOR("io_basic:getct1", 2);
  int i;
  
  i = readchar(Input_Stream_Ptr,GET1,address_getct1); /* skip whitespace */

  if (i == CHAR_PAST_EOF) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
  }

  return cunify(Arg,X(0),MakeSmall(i)) 
         && cunify(Arg,X(1),MakeSmall(i == CHAR_EOF ? -1 : symbolchar[i]));
}


/*----------------------------------------------------------------*/

CBOOL__PROTO(get)
{
  ERR__FUNCTOR("io_basic:get_code", 1);
  int i;

  i = readchar(Input_Stream_Ptr,GET,address_get);

  if (i == CHAR_PAST_EOF) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
  }

  return cunify(Arg,X(0),MakeSmall(i));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(get2)
{
  ERR__FUNCTOR("io_basic:get_code", 2);
  int i;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }

  i = readchar(s,GET,address_get2);

  if (i == CHAR_PAST_EOF) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),X(0),1);
  }

  return cunify(Arg,X(1),MakeSmall(i));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(get1)
{
  ERR__FUNCTOR("io_basic:get1_code", 1);
  int i;
  
  i = readchar(Input_Stream_Ptr,GET1,address_get1); /* skip whitespace */

  if (i == CHAR_PAST_EOF) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
  }

  return cunify(Arg,X(0),MakeSmall(i));
}


/*----------------------------------------------------------------*/


CBOOL__PROTO(get12)
{
  ERR__FUNCTOR("io_basic:get1_code", 2);
  int i;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }

  i = readchar(s,GET1,address_get12); /* skip whitespace */

  if (i == CHAR_PAST_EOF) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),X(0),1);
  }

  return cunify(Arg,X(1),MakeSmall(i));
}


/*----------------------------------------------------------------*/

CBOOL__PROTO(peek)
{
  ERR__FUNCTOR("io_basic:peek_code", 1);
  int i;

  i = readchar(Input_Stream_Ptr,PEEK,address_peek);

  if (i == CHAR_PAST_EOF) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
  }

  return cunify(Arg,X(0),MakeSmall(i));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(peek2)
{
  ERR__FUNCTOR("io_basic:peek_code", 2);
  int i;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }

  i = readchar(s,PEEK,address_peek2);

  if (i == CHAR_PAST_EOF) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),X(0),1);
  }

  return cunify(Arg,X(1),MakeSmall(i));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(nl)
{
  writechar('\n',Output_Stream_Ptr);
  return TRUE;
}


/*----------------------------------------------------------------*/

CBOOL__PROTO(nl1)
{
  ERR__FUNCTOR("io_basic:nl", 1);
  int errcode;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  writechar('\n',s);
  return TRUE;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(put)
{
  ERR__FUNCTOR("io_basic:put_code", 1);
  int i;

  DEREF(X(0), X(0));
  CheckGetCharacterCode(X(0),i,1);
  writechar(i,Output_Stream_Ptr);

  return TRUE;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(put2)
{
  ERR__FUNCTOR("io_basic:put_code", 2);
  int i;
  stream_node_t *s;

  s = stream_to_ptr_check(X(0), 'w', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }

  DEREF(X(1), X(1));
  CheckGetCharacterCode(X(1),i,2);
  writechar(i,s);

  return TRUE;
}

/*----------------------------------------------------------------*/
/* output stream always write or append */

CBOOL__PROTO(tab)
{
  ERR__FUNCTOR("io_basic:tab", 1);
  DEREF(X(0),X(0));
  if (!IsInteger(X(0))) {
    ERROR_IN_ARG(X(0),1,INTEGER);
  }

  writecharn(' ',GetInteger(X(0)),Output_Stream_Ptr);
  return TRUE;
}


/*----------------------------------------------------------------*/

CBOOL__PROTO(tab2)
{
  ERR__FUNCTOR("io_basic:tab", 2);
  int errcode;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  DEREF(X(1),X(1));
  if (!IsInteger(X(1))) {
    ERROR_IN_ARG(X(1),2,INTEGER);
  }

  writecharn(' ',GetInteger(X(1)),s);
  return TRUE;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(skip)
{
  ERR__FUNCTOR("io_basic:skip_code", 1);
  int i, ch;

  DEREF(X(0),X(0));
  CheckGetCharacterCode(X(0),i,1);

  for (ch=i+1; ch!=i && ch != CHAR_PAST_EOF;)
    ch = readchar(Input_Stream_Ptr,i,address_skip);

  if (ch == CHAR_PAST_EOF) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
  }

  return TRUE;
}

/*----------------------------------------------------------------*/


CBOOL__PROTO(skip2)
{
  ERR__FUNCTOR("io_basic:skip_code", 2);
  int i, ch;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }

  DEREF(X(1),X(1))
  CheckGetCharacterCode(X(1),i,2);

  for (ch=i+1; ch!=i && ch != CHAR_PAST_EOF;)
    ch = readchar(s,i,address_skip2);

  if (ch == CHAR_PAST_EOF) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),X(0),1);
  }

  return TRUE;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(skip_line)
{
  // ERR__FUNCTOR("io_basic:skip_line", 0);
  int ch;

  for (ch=0; ch!=0xa && ch!=0xd && ch>=0;)
    ch = readchar(Input_Stream_Ptr,SKIPLN,address_skip_line);

  if (ch == 0xd) /* Delete a possible 0xa (win end-of-line) */
    readchar(Input_Stream_Ptr,DELRET,address_skip_line);

  return TRUE;
}

/*----------------------------------------------------------------*/


CBOOL__PROTO(skip_line1)
{
  ERR__FUNCTOR("io_basic:skip_line", 1);
  int errcode, ch;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  for (ch=0; ch!=0xa && ch!=0xd && ch>=0;)
    ch = readchar(s,SKIPLN,address_skip_line1);

  if (ch == 0xd) /* Delete a possible 0xa (win end-of-line) */
    readchar(s,DELRET,address_skip_line1);

  return TRUE;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(get_byte1)
{
  ERR__FUNCTOR("io_basic:get_byte", 1);
  int i;

  i = readbyte(Input_Stream_Ptr,address_get);

  if (i == CHAR_PAST_EOF) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
  }

  return cunify(Arg,X(0),MakeSmall(i));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(get_byte2)
{
  ERR__FUNCTOR("io_basic:get_byte", 2);
  int i;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }

  i = readbyte(s,address_get2);

  if (i == CHAR_PAST_EOF) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),X(0),1);
  }

  return cunify(Arg,X(1),MakeSmall(i));
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(put_byte1)
{
  ERR__FUNCTOR("io_basic:put_byte", 1);
  int i;

  DEREF(X(0),X(0));
  CheckGetByte(X(0),i,1);;
  writebyte(i,Output_Stream_Ptr);

  return TRUE;
}

/*----------------------------------------------------------------*/

CBOOL__PROTO(put_byte2)
{
  ERR__FUNCTOR("io_basic:put_byte", 2);
  int i;
  stream_node_t *s;

  s = stream_to_ptr_check(X(0), 'w', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }

  DEREF(X(1),X(1));
  CheckGetByte(X(1),i,2);;
  writechar(i,s);

  return TRUE;
}

/*----------------------------------------------------------------*/
/* NOTE: Moved from streams_basic.c (DCG) */
/*----------------------------------------------------------------*/

/* This is essentially an open-coded fputs().  
   fputs() starts paying off at string lengths above 50 or so.
 */
void print_string(stream_node_t *stream,
		  char *p)
{
  FILE *fileptr = stream->streamfile;
  int i;

  if (stream->isatty) {
    stream = root_stream_ptr;
    for (i = *p++; i; i = *p++) {
      /* ignore errors on tty */
      putc(i,fileptr);
      inc_counts(i,stream);
    }
  } else if (stream->streammode != 's') {                      /* Is not a socket */
    for (i = *p++; i; i = *p++) {
      if (putc(i,fileptr) < 0) {
	IO_ERROR("putc() in in print_string()");
      }
      inc_counts(i,stream);
    }
  } else {
    size_t size = 0;
    char *q = p;

    for (i = *q++; i; i = *q++) {
      inc_counts(i,stream);
      size++;
    }
    if (write(GetInteger(stream->label), p, size) < 0) {
      IO_ERROR("write() in print_string()");
    }
  }
  fflush(fileptr);
}

CVOID__PROTO(print_variable, stream_node_t *stream, tagged_t term)
{
  number_to_string(Arg, var_address(Arg, term), 10);
  print_string(stream, "_");
  print_string(stream, Atom_Buffer);
}

CVOID__PROTO(print_number, stream_node_t *stream, tagged_t term)
{
  number_to_string(Arg,term, 10);
  print_string(stream, Atom_Buffer);
}

#define FULL_ESCAPE_QUOTED_ATOMS 1

#define PRINT_CONTROL_CHAR(X) { *bp++ = '\\'; *bp++ = (X); }

CVOID__PROTO(print_atom, stream_node_t *stream, tagged_t term)
{
  atom_t *atomptr = TagToAtom(term);
  
  if (!atomptr->has_special)
    print_string(stream, atomptr->name);
  else
    {
#if defined(USE_DYNAMIC_ATOM_SIZE)
      char *buf = checkalloc_ARRAY(char, 2*MAXATOM+3);
#else
      char buf[2*MAXATOM+3]; 
#endif
      unsigned char *ch = (unsigned char *)atomptr->name;
      char *bp = buf;
      int i;
      
      *bp++ = '\'';
#if defined(FULL_ESCAPE_QUOTED_ATOMS)
      while ((i = *ch++)) {
	extern char symbolchar[256];

	/* See tokenize.pl for table of symbolic control chars */
	if (symbolchar[i] == 0) {
	  switch (i) {
	  case 7: PRINT_CONTROL_CHAR('a'); break;
	  case 8: PRINT_CONTROL_CHAR('b'); break;
	  case 9: PRINT_CONTROL_CHAR('t'); break;
	  case 10: PRINT_CONTROL_CHAR('n'); break;
	  case 11: PRINT_CONTROL_CHAR('v'); break;
	  case 12: PRINT_CONTROL_CHAR('f'); break;
	  case 13: PRINT_CONTROL_CHAR('r'); break;
	  /* case 27: PRINT_CONTROL_CHAR('e'); break; */
	  case 32: *bp++ = ' '; break;
	  /* case 127: PRINT_CONTROL_CHAR('d'); break; */
	  default:
	    *bp++ = '\\';
	    *bp++ = '0' + ((i >> 6) & 7);
	    *bp++ = '0' + ((i >> 3) & 7);
	    *bp++ = '0' + (i & 7);
	    *bp++ = '\\';
	  }
	} else {
	  if (i=='\'' || i=='\\')
	    *bp++ = i;
	  *bp++ = i;
	}
      }
#else
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
#endif
      *bp++ = '\'';
      *bp++ = 0;
      print_string(stream, buf);
#if defined(USE_DYNAMIC_ATOM_SIZE)
      checkdealloc_ARRAY(char, 2*MAXATOM+3, buf);
#endif
    }
}

/*   --------------------------------------------------------------  */	 

CVOID__PROTO(display_term,
	     tagged_t term,
	     stream_node_t *stream,
	     bool_t quoted)
{
  tagged_t aux;
  int arity,i;

  switch (TagOf(term)) {
  case LST:
    writechar('[',stream);
    DerefCar(aux,term);
    display_term(Arg,aux, stream, quoted);
    DerefCdr(term,term);
    while(TagIsLST(term)) {
      writechar(',',stream);
      DerefCar(aux,term);
      display_term(Arg,aux, stream, quoted);
      DerefCdr(term,term);
    }
    if (term!=atom_nil){
      writechar('|',stream);
      display_term(Arg,term, stream, quoted);
    }
    writechar(']',stream);
    break;
  case STR:
    if (STRIsLarge(term))
      goto number;
    display_term(Arg,TagToHeadfunctor(term),stream, quoted);
    writechar('(',stream);
    arity = Arity(TagToHeadfunctor(term));
    for(i=1; i<=arity; i++){
      if (i>1) writechar(',',stream);
      DerefArg(aux,term,i);
      display_term(Arg,aux, stream, quoted);
    }
    writechar(')',stream);
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
      print_atom(Arg,stream,term);
    else
      print_string(stream,TagToAtom(term)->name);
    break;
  case NUM:
  number:
  print_number(Arg,stream,term);
  break;
  }
}

CBOOL__PROTO(prolog_display)
{
  DEREF(X(0),X(0));
  display_term(Arg,X(0),Output_Stream_Ptr, FALSE);
  return TRUE;
}

CBOOL__PROTO(prolog_display2)
{
  ERR__FUNCTOR("io_basic:display", 2);
  int errcode;
  stream_node_t *stream;
  
  stream = stream_to_ptr_check(X(0), 'w', &errcode);
  if (stream==NULL) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  DEREF(X(1),X(1));
  display_term(Arg,X(1),stream, FALSE);
  return TRUE;
}

CBOOL__PROTO(prolog_displayq)
{
  DEREF(X(0),X(0));
  display_term(Arg,X(0), Output_Stream_Ptr, TRUE);
  return TRUE;
}

CBOOL__PROTO(prolog_displayq2)
{
  ERR__FUNCTOR("io_basic:displayq", 2);
  int errcode;
  stream_node_t *stream;
  
  stream = stream_to_ptr_check(X(0), 'w', &errcode);
  if (stream==NULL) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  DEREF(X(1),X(1));
  display_term(Arg,X(1), stream, TRUE);
  return TRUE;
}

CBOOL__PROTO(prolog_clearerr)
{
  ERR__FUNCTOR("streams_basic:clearerr", 1);
  int errcode;
  stream_node_t *s;
  
  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s) {
    BUILTIN_ERROR(errcode,X(0),1);
  }
  
  if (s->streammode != 's') clearerr(s->streamfile);

  return TRUE;
}

/*----------------------------------------------------*/

#define FASTRW_VERSION  'C'
#define FASTRW_MAX_VARS 1024

#define SPACE_FACTOR 64  /* kludge to ensure more heap space before reading */

CBOOL__PROTO(prolog_fast_read_in_c_aux, 
	     tagged_t *out,
	     tagged_t *vars,
	     int *lastvar);

/* OPA */
CBOOL__PROTO(prolog_fast_read_in_c)
{
  ERR__FUNCTOR("fastrw:fast_read", 1);
  int i,lastvar = 0;
  tagged_t term, vars[FASTRW_MAX_VARS];


/* MCL, JC: Changed getc() to readbyte() because of wrong assumptions when
   using sockets (i.e., streamfile = NULL.  */

 /* NULL as predaddress (really did not bother to find out what to put)  */

  i = readbyte(Input_Stream_Ptr, NULL);
  if (i == CHAR_PAST_EOF) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
  }
  if (i != FASTRW_VERSION) return FALSE;

  if (HeapDifference(w->global_top,Heap_End) < CONTPAD+SPACE_FACTOR*kCells)
        explicit_heap_overflow(Arg,CONTPAD+SPACE_FACTOR*kCells,1);

  if (!prolog_fast_read_in_c_aux(Arg,&term,vars,&lastvar)) return FALSE;

  return cunify(Arg,X(0),term);
}

#if defined(DEBUG)
#define CHECK_HEAP_SPACE \
  if (HeapDifference(w->global_top,Heap_End) < CONTPAD) \
     fprintf(stderr, "Out of heap space in fast_read()\n")
#else
#define CHECK_HEAP_SPACE
#endif

CBOOL__PROTO(prolog_fast_read_in_c_aux, 
	     tagged_t *out,
	     tagged_t *vars,
	     int *lastvar)
{
  ERR__FUNCTOR("fastrw:fast_read", 1);
  int i,k,j;
  unsigned char *s = (unsigned char *) Atom_Buffer;
  tagged_t *h = w->global_top;
  int base;
  
  k = readbyte(Input_Stream_Ptr, NULL);
  if (k == CHAR_PAST_EOF) {
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
  }

  switch(k) {
  case ']':
    *out = atom_nil;
    CHECK_HEAP_SPACE;
    return TRUE;
  case '[':
    w->global_top += 2;
    if (!prolog_fast_read_in_c_aux(Arg,h,vars,lastvar)) return FALSE;
    if (!prolog_fast_read_in_c_aux(Arg,h+1,vars,lastvar)) return FALSE;
    *out = Tag(LST,h);
    CHECK_HEAP_SPACE;
    return TRUE;
  case '_':
  case 'I':
  case 'F':
  case 'A':
  case '"':
  case 'S':
    j = 1;
    for (i=0; j; i++) {
      if (i == Atom_Buffer_Length) {
        EXPAND_ATOM_BUFFER(Atom_Buffer_Length*2);
	s = (unsigned char *)Atom_Buffer+i;
      }
      j = readbyte(Input_Stream_Ptr, NULL);
      if (j == CHAR_PAST_EOF) {
	BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
      }
      *s++ = j;
    }
    switch (k) {
    case '_':
      if ((i = atoi(Atom_Buffer)) == *lastvar)
	*h = vars[(*lastvar)++] = TagHVA(w->global_top++);
      *out = vars[i];
      CHECK_HEAP_SPACE;
      return TRUE;
    case 'I':
      base = GetSmall(current_radix);
      if ((i = bn_from_string(Atom_Buffer, (bignum_t *)h, (bignum_t *)(Heap_End-CONTPAD), base))) {
	explicit_heap_overflow(Arg,i+CONTPAD, 1);
        h = w->global_top;        
	if (bn_from_string(Atom_Buffer, (bignum_t *)h, (bignum_t *)(Heap_End-CONTPAD), base))
	  SERIOUS_FAULT("miscalculated size of bignum");
      }
      if ((i = LargeArity(h[0])) ==2 && IntIsSmall((intmach_t)h[1])) // TODO: This assumes that sizeof(bignum_t) == sizeof(intmach_t) == sizeof(tagged_t)
	*out = MakeSmall(h[1]);
      else {
	*out = Tag(STR,h);
	w->global_top += i+1;
	h[i] = h[0];
      }	
      CHECK_HEAP_SPACE;
      return TRUE;
    case 'F':
      string_to_number(Arg, Atom_Buffer, 10, out, 2);
      CHECK_HEAP_SPACE;
      return TRUE;
    case 'A':
      *out = MakeString(Atom_Buffer);
      CHECK_HEAP_SPACE;
      return TRUE;
    case '"':
      i--;
      /*
      if (HeapDifference(w->global_top,Heap_End)<CONTPAD+(i<<1)){
        printf("Prev HeapDifference is %d\n",
               HeapDifference(w->global_top,Heap_End));
        explicit_heap_overflow(Arg,CONTPAD+(i<<1),1);
      }
      */
      while (i--) MakeLST(*out,MakeSmall(((unsigned char *) Atom_Buffer)[i]),*out);
      if (!prolog_fast_read_in_c_aux(Arg,h+1,vars,lastvar)) return FALSE;
      CHECK_HEAP_SPACE;
      return TRUE;
    case 'S':
      i = readbyte(Input_Stream_Ptr, NULL);
      if (i == CHAR_PAST_EOF) {
	BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0);
      }
          /*
      if (HeapDifference(w->global_top,Heap_End)<CONTPAD+(i+1))
        explicit_heap_overflow(Arg,CONTPAD+(i+1),1);
          */
      *h = SetArity(MakeString(Atom_Buffer),i);
      *out = Tag(STR,h++);
      w->global_top += i+1;
      while(i--)
	if (!prolog_fast_read_in_c_aux(Arg,h++,vars,lastvar)) return FALSE;
      CHECK_HEAP_SPACE;
      return TRUE;
    }
  default:
    return FALSE;
  }
}

static inline void fast_write_string(stream_node_t *stream,
				     const char * s) {
  for(;*s; s++) writebyte(*s, stream);
}

CVOID__PROTO(fast_write_number,
	     stream_node_t *stream,
	     tagged_t term) {
  number_to_string(Arg,term, 10);
  fast_write_string(stream, Atom_Buffer);
}

CVOID__PROTO(prolog_fast_write_in_c_aux,
	     tagged_t in,
	     tagged_t *vars, 
	     int *lastvar);

/* OPA */
CBOOL__PROTO(prolog_fast_write_in_c)
{
  tagged_t vars[FASTRW_MAX_VARS];
  int lastvar = 0;

  DEREF(X(0),X(0));
  writebyte(FASTRW_VERSION,Output_Stream_Ptr);
  prolog_fast_write_in_c_aux(Arg,X(0),vars,&lastvar);
  return TRUE;
}

CVOID__PROTO(prolog_fast_write_in_c_aux,
	     tagged_t in,
	     tagged_t *vars,
	     int *lastvar)
{
  int i,j;
  tagged_t term;

  switch (TagOf(in))
    {
    case LST:
      DerefCar(term,in);
      DerefCdr(in,in);
      if (TagIsSmall(term) && (i = GetSmall(term)))
	if ((i > 0) && (i < 256)) {
	  for(writebyte('"',Output_Stream_Ptr);i && (i < 256);) {
	    writebyte(i,Output_Stream_Ptr);
	    if (TagOf(in) == LST) {
	      DerefCar(term,in);
	      DerefCdr(in,in);
	      if (!TagIsSmall(term)) break;
	      else i = GetSmall(term);
	    }
	    else {
	      writebyte(0,Output_Stream_Ptr);
	      prolog_fast_write_in_c_aux(Arg,in,vars,lastvar);
	      return;
	    }	  
	  }
	  writebyte(0,Output_Stream_Ptr);
	}
      writebyte('[',Output_Stream_Ptr);
      prolog_fast_write_in_c_aux(Arg,term,vars,lastvar);
      prolog_fast_write_in_c_aux(Arg,in,vars,lastvar);
      return;
    case UBV:
    case SVA:
    case HVA:
    case CVA:
      writebyte('_',Output_Stream_Ptr);
      DEREF(in,in);
      for (i = 0;i < *lastvar; i++)
	if (vars[i] == in) break;
      if (i == *lastvar) vars[(*lastvar)++] = in;
      sprintf((char *) Atom_Buffer,"%i",i);
      fast_write_string(Output_Stream_Ptr,Atom_Buffer);
      writebyte(0,Output_Stream_Ptr);
      return;
    case STR:
      if (!STRIsLarge(in)) {
      writebyte('S',Output_Stream_Ptr);
      fast_write_string(Output_Stream_Ptr,TagToAtom(TagToHeadfunctor(in))->name);
      writebyte(0,Output_Stream_Ptr);
      writebyte(j = Arity(TagToHeadfunctor(in)),Output_Stream_Ptr);
      for(i = 1; i <= j; prolog_fast_write_in_c_aux(Arg,term,vars,lastvar))
	DerefArg(term,in,i++);
      return;
      }
    case NUM:
      if (IsFloat(in)) writebyte('F',Output_Stream_Ptr);
      else writebyte('I',Output_Stream_Ptr);
      fast_write_number(Arg,Output_Stream_Ptr,in);
      writebyte(0,Output_Stream_Ptr);
      return;
    case ATM:
      if (in != atom_nil) {
	writebyte('A',Output_Stream_Ptr);
	fast_write_string(Output_Stream_Ptr,TagToAtom(in)->name);
	writebyte(0,Output_Stream_Ptr);
      }
      else writebyte(']',Output_Stream_Ptr);
      return;
    }
}

/*----------------------------------------------------*/

/* Routines for the compression and uncompression of bytecode, used on 
   the CIAO compiler (OPA) */ 

unsigned char sizeLZ(int n)
{ if (n > 2047) return 12;
  else if (n > 1023) return 11;
  else if (n > 511) return 10;
  else return 9;}

CVOID__PROTO(outLZ,
	     int *Buffer,
	     char *BufferSize,
	     int Code,
	     unsigned char size)
{
  Buffer[0] += Code*(1<<(BufferSize[0]));
  for(BufferSize[0] += size; BufferSize[0] >= 8; BufferSize[0] -= 8) {
    writebyte(Buffer[0] % 256,Output_Stream_Ptr);
    Buffer[0] /= 256;
  }
}

CBOOL__PROTO(compressLZ)
{
  ERR__FUNCTOR("compressed_bytecode:compressLZ", 1);
  char *Dict[4096];
  char *First;
  char Vault[200000];
  char CarrySize = 0;
  int  i;
  int  Carry = 0;  
  int  Last = 256;
  int  PrefixSize = 0;
  int  Entry = 0;
  int  Size[4096];
  stream_node_t *s;
  FILE *f;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }

  f = s->streamfile;
  
  writebyte(12,Output_Stream_Ptr);

  for (i = 0; i < 257; Size[i++] = 1) 
      { Dict[i] = &Vault[i];
        Vault[i] = i % 256; }
  First = &Vault[256];
  
  while((i = getc(f)) >= 0) {
    First[PrefixSize++] = i;
    for (i = Entry; Entry <= Last; Entry++) 
      if ((Size[Entry] == PrefixSize) && (Dict[Entry][0] == First[0])
          && !(memcmp(&Dict[Entry][1],&First[1],PrefixSize-1))) break;
    if (Entry > Last) {
      Entry = First[PrefixSize-1];
      outLZ(Arg,&Carry,&CarrySize,i,sizeLZ(Last));
      if (Last == 4095) First = &Vault[Last = 256];
      else {
	Dict[++Last] = First;
	Size[Last] = PrefixSize;
	First += PrefixSize; }
      First[0] = Entry;
      PrefixSize = 1;}}

  if (ferror(f)) {
    IO_ERROR("getc() in compressLZ");
  }

  if (PrefixSize) outLZ(Arg,&Carry,&CarrySize,Entry,sizeLZ(Last));
  outLZ(Arg,&Carry,&CarrySize,256,sizeLZ(Last));
  outLZ(Arg,&Carry,&CarrySize,0,7);
  return TRUE;
}

static CVOID__PROTO(inLZ, FILE *f,
		    int *Buffer, char *BufferSize,
		    int *Code, char size)
{
  //  ERR__FUNCTOR("compressed_bytecode:copyLZ", 1);
  int i;

  for (; BufferSize[0] < size; BufferSize[0] += 8) {
    i = getc(f);
    if (i < 0) {
      if (ferror(f)) {
	IO_ERROR("getc() in inLZ()");
      }
    } 
    Buffer[0] += ((unsigned char) i)*(1<<BufferSize[0]);
  }
  Code[0] = Buffer[0] % (1<<size);
  Buffer[0] /= (1<<size);
  BufferSize[0] -= size;
}
      
CBOOL__PROTO(copyLZ)
{
  ERR__FUNCTOR("compressed_bytecode:copyLZ", 1);
  int  i;
  int  Last = 256;
  int  PrefixSize = 1;
  int  Carry = 0;
  char CarrySize = 0;
  char *Dict[4096];
  int  Size[4096];
  char *First;
  char Vault[200000];
  stream_node_t *s;
  FILE *f;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) {
    BUILTIN_ERROR(i,X(0),1);
  }

  f = s->streamfile;
  
  i = getc(f);
  
  if (i != 12) {
    while (i >= 0) {
      writebyte(i,Output_Stream_Ptr);
      i = getc(f);
    }
    if (ferror(f)) IO_ERROR("getc() in copyLZ()");
    return TRUE;
  } else {
    for (i = 0; i < 257; Size[i++] = 1) {
      Dict[i] = &Vault[i];
      Vault[i] = i % 256;
    }
    First = &Vault[256];
 
    inLZ(Arg,f,&Carry,&CarrySize,&i,9);
    First[0] = i;
    while(1) {
      for (i = 0; i < PrefixSize;) writebyte(First[i++],Output_Stream_Ptr);
    inLZ(Arg,f,&Carry,&CarrySize,&i,sizeLZ(++Last % 4096));
	return FALSE;
      if (i == 256) return TRUE;
      if (Last == 4096) {
        (First = &Vault[Last = 256])[0] = i;
	PrefixSize = 1;
      } else {
	Size[Last] = PrefixSize+1;            
	(Dict[Last] = First)[PrefixSize] = Dict[i][0];
	(void)memmove(First += Size[Last],Dict[i],PrefixSize = Size[i]);
      }
    }
  }
  return FALSE;
}