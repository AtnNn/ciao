/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#include "datadefs.h"
#include "support.h"

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

# include <sys/param.h>
# include <sys/errno.h>

#if !defined(MAXPATHLEN)
# if defined(PATH_MAX)
#  define MAXPATHLEN PATH_MAX
# else
#  define MAXPATHLEN 1024
# endif
#endif

#define ENG_NOFILES 20

/* declarations for global functions accessed here */

#include "alloc_defs.h"
#include "support_defs.h"
#include "initial_defs.h"
#include "nondet_defs.h"

/* local declarations */

/* void ENG_perror(); */

/* ----------------------------------------------------------------------- */
/* The root stream pointer (all streams are liked) */

stream_node_t *root_stream_ptr;               /* Shared and _locked_ */

/* ----------------------------------------------------------------------- */
/* The table of stream aliases */

stream_node_t *stream_user_input  = NULL;                  /* Shared */
stream_node_t *stream_user_output = NULL;                  /* Shared */
stream_node_t *stream_user_error  = NULL;                  /* Shared */

tagged_t atom_user;		/* "user" */

tagged_t atom_user_input;	/* "user_input" */ 
tagged_t atom_user_output;	/* "user_output" */
tagged_t atom_user_error;	/* "user_error" */

/* ----------------------------------------------------------------------- */
/* Initialize the streams library (only once) */

void init_streams()
{
  root_stream_ptr = 
    (stream_node_t *)checkalloc(sizeof(stream_node_t)); 
  root_stream_ptr->label=ERRORTAG;
  root_stream_ptr->streamname=ERRORTAG;
  root_stream_ptr->forward=root_stream_ptr;
  root_stream_ptr->backward=root_stream_ptr;
  root_stream_ptr->last_nl_pos = 0;               /* used for tty streams */
  root_stream_ptr->nl_count = 0;
  root_stream_ptr->char_count = 0;

  stream_user_input = new_stream(ERRORTAG, "r", stdin);
  stream_user_output = new_stream(ERRORTAG, "a", stdout);
  stream_user_error = new_stream(ERRORTAG, "a", stderr);

  atom_user=init_atom_check("user");

  /* initialize the table of streams aliases */
  atom_user_input=init_atom_check("user_input");
  atom_user_output=init_atom_check("user_output");
  atom_user_error=init_atom_check("user_error");
}


/* --------------------------------------------------------*/

/* Protect the creation of streams: several threads might want to create
   streams at once. */

extern LOCK stream_list_l;

stream_node_t *insert_new_stream(stream_node_t *new_stream){

  Wait_Acquire_lock(stream_list_l);
  new_stream->forward = root_stream_ptr;
  new_stream->backward = root_stream_ptr->backward;
  root_stream_ptr->backward->forward = new_stream;
  root_stream_ptr->backward = new_stream;
  Release_lock(stream_list_l);

  return new_stream;
}

void update_stream(register stream_node_t *s, FILE *file);

stream_node_t *new_stream(streamname, streammode, streamfile)
     tagged_t streamname;
     char *streammode;
     FILE *streamfile;
{
  CIAO_REGISTER stream_node_t *s;

  s = (stream_node_t *)checkalloc(sizeof(stream_node_t));
  s->streamname = streamname;
  s->streammode = streammode[0];
  s->pending_char = -100;
  s->socket_eof = FALSE;
  update_stream(s,streamfile);

  return insert_new_stream(s);
}


static int file_is_tty(file)
     FILE *file;
{
  extern int prolog_force_interactive;

  return (isatty(fileno(file)) ||
          (prolog_force_interactive && fileno(file)<3));
}


void update_stream(s,file)
     CIAO_REGISTER stream_node_t *s;
     FILE *file;
{
  s->label = MakeSmall(fileno(file));
  s->streamfile = file;
  if ((s->isatty = file_is_tty(file)))
    s = root_stream_ptr;
  s->last_nl_pos = 0;
  s->nl_count = 0;
  s->char_count = 0;		/* less than perfect */
}

#if 0 /* Not used */
#if defined(CREATE_NEW_STREAMS)
void update_std_streams()		/* called by restore/1 */
{
  struct
    stream_node *streamptr = root_stream_ptr, *next_ptr;

  do {
    next_ptr = streamptr->forward;
    if (streamptr->streamname!=ERRORTAG)
      fclose(streamptr->streamfile);
    checkdealloc(streamptr);
    streamptr = next_ptr;
  } while (streamptr!=root_stream_ptr);
  init_streams();
  init_streams_each_time(Arg);
}
#else
void update_std_streams()		/* called by restore/1 */
{
  stream_node_t *streamptr = root_stream_ptr->forward;

  while (streamptr!=root_stream_ptr) {         /* close any ghost streams */
    if (streamptr->streamname!=ERRORTAG)
      fclose(streamptr->streamfile);
    else			      /* check if std stream is a tty now */
      update_stream(streamptr,streamptr->streamfile);
    streamptr = streamptr->forward;
  }
}
#endif
#endif

/* ----------------------------------------------------------------------- */
/* Functions to check types */
/* (useful to find when exceptions should be raised) */

/* ISO Prolog does not allow stream aliases to be used instead of
   stream terms in many cases.  We are relaxing this here. */

bool_t is_var_or_alias_or_stream(Arg, Cell)
     Argdecl;
     tagged_t Cell;
{
  if (IsVar(Cell)) {
    /* a variable */
    return TRUE;
  } else if (TagIsATM(Cell)) {
    /* a stream alias */
    return (Cell == atom_user_input ||
	    Cell == atom_user_output ||
	    Cell == atom_user_error);
  } else {
    /* a Dstream functor */
    return (TagIsSTR(Cell) && TagToHeadfunctor(Cell) == functor_Dstream);
  }

}

/* ----------------------------------------------------------------------- */
/* '$stream'(<address>,<id>) <-- (stream_node_t *) */

tagged_t ptr_to_stream_noalias(Arg, n)
     Argdecl;
     CIAO_REGISTER stream_node_t *n;
{
  CIAO_REGISTER tagged_t *pt1 = w->global_top;

  /*
  printf("(int)n is %ud\n", (int)n);
  printf("n->label is %ud\n", n->label);
  */

  HeapPush(pt1,functor_Dstream);
  HeapPush(pt1,PointerToTerm(n));
  HeapPush(pt1,n->label);
  return (w->global_top=pt1, Tag(STR,HeapOffset(pt1,-3)));
}

/* ----------------------------------------------------------------------- */
/* '$stream'(<address>,<id>) <-- (stream_node_t *) */
/* or */
/* <stream_alias> <-- (stream_node_t *) */
tagged_t ptr_to_stream(Arg,n)
     Argdecl;
     CIAO_REGISTER stream_node_t *n;
{
  if (n==stream_user_input)
    return atom_user_input;
  if (n==stream_user_output)
    return atom_user_output;
  if (n==stream_user_error)
    return atom_user_error;

  return ptr_to_stream_noalias(Arg, n);
}

/* ----------------------------------------------------------------------- */
/* '$stream'(<address>,<id>) --> (stream_node_t *)
                          --> NULL, if invalid
   'r' - read mode,  streammode=[rs]
   'w' - write mode, streammode=[was]
   'x' - any mode,   streammode=[rwas]
   'y' - any mode but no standard streams  */
stream_node_t *stream_to_ptr(t, mode)
     CIAO_REGISTER tagged_t t;
     int mode;
{
  CIAO_REGISTER stream_node_t *n = NULL;
  CIAO_REGISTER tagged_t x1, x2;

  DerefSwitch(t,x1,;);

  if (TagIsATM(t))
    {
      if (mode=='y')
	n = NULL;
      else if (t==atom_user)
	n = (mode=='r' ? stream_user_input : stream_user_output);
      else if (t==atom_user_input)
	n = stream_user_input;
      else if (t==atom_user_output)
	n = stream_user_output;
      else if (t==atom_user_error)
	n = stream_user_error;
    }
  else if (TagIsSTR(t) && (TagToHeadfunctor(t) == functor_Dstream))
    {
      DerefArg(x1,t,1);
      DerefArg(x2,t,2);
      if (!TagIsSmall(x1) || !TagIsSmall(x2) ||
	  (n=TagToStream(x1), n->label != x2))
	n = NULL;
    }

  if (((mode=='r') && (n!=NULL) && (n->streammode=='w'||n->streammode=='a')) ||
      ((mode=='w') && (n!=NULL) && (n->streammode=='r')))
    return NULL;
  else
    return n;
}

/* ----------------------------------------------------------------------- */
/* Similar to stream_to_ptr(), but giving an error code */

stream_node_t *stream_to_ptr_check(t, mode, errcode)
     CIAO_REGISTER tagged_t t;
     int mode; /* not 'y' */
     int *errcode;
{
  CIAO_REGISTER stream_node_t *n = NULL;
  CIAO_REGISTER tagged_t x1, x2;

  DerefSwitch(t,x1,{*errcode = INSTANTIATION_ERROR; return NULL;});

  if (TagIsATM(t))
    {
      if (t==atom_user)
	n = (mode=='r' ? stream_user_input : stream_user_output);
      else if (t==atom_user_input)
	n = stream_user_input;
      else if (t==atom_user_output)
	n = stream_user_output;
      else if (t==atom_user_error)
	n = stream_user_error;
      else {
            *errcode = DOMAIN_ERROR(STREAM_OR_ALIAS);
            return NULL;
      }
    }
  else if (TagIsSTR(t) && (TagToHeadfunctor(t) == functor_Dstream)) {
      DerefArg(x1,t,1);
      DerefArg(x2,t,2);
      if (!TagIsSmall(x1) || !TagIsSmall(x2) ||
	  (n = TagToStream(x1), n->label != x2)) {
            *errcode = EXISTENCE_ERROR(STREAM);
            return NULL;
      }
    } else {
        *errcode = DOMAIN_ERROR(STREAM_OR_ALIAS);
        return NULL;
    }

  if (mode=='r') {
    if (n->streammode=='w'||n->streammode=='a') {
      *errcode = PERMISSION_ERROR(ACCESS,STREAM);  
      return NULL;
    }
  } else if (mode=='w') {
    if (n->streammode=='r') {
      *errcode = PERMISSION_ERROR(MODIFY,STREAM); 
      return NULL;
    }
  }

  return n;
}

/* ----------------------------------------------------------------------- */
/* BUILTIN C PREDICATES */

/* ----------------------------------------------------------------------- */
/* USAGE: open(+,+,-) only */

bool_t prolog_open(Arg)
     Argdecl;
{
  ERR__FUNCTOR("streams_basic:$open", 3);
  struct stat statbuf;
  FILE *fileptr;
  char *modecodif, modespec[2];
  extern int errno;
  enum {
    STRANGE_SOURCE_SINK,
    INEXISTENT_SOURCE_SINK,
    CANNOT_OPEN,
    SYS_ERROR,
    FINISHED_RESOURCES
  } what_happened;


  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  modecodif = GetString(X(1));

  modespec[0] = modecodif[0];
  modespec[1] = 0;

  fileptr = (TagIsATM(X(0))   ?  fopen(GetString(X(0)), modespec) :
	     TagIsSmall(X(0)) ? fdopen(GetSmall(X(0)),  modespec) :
	     NULL);

  if (fileptr==NULL) {
    what_happened = SYS_ERROR;                            /* Just in case */
    if (errno==ENOENT || errno==ENOTDIR || errno==ENXIO ||errno==EBADF)
      what_happened = INEXISTENT_SOURCE_SINK;
    else if (errno==EEXIST || errno==EISDIR || 
               errno==EBADF || errno==EROFS)
      what_happened = CANNOT_OPEN;
    else if (errno==ENOMEM || errno==EMFILE || errno==ENFILE)
      what_happened = FINISHED_RESOURCES;
    goto bombit;
    } else {
        if (fstat(fileno(fileptr), &statbuf) || 
            (statbuf.st_mode & S_IFMT) == S_IFDIR) {
          fclose(fileptr);
          what_happened = CANNOT_OPEN;
          goto bombit;
        }
    }

  {
    char locking = modecodif[1];

    if (locking == 'l' || locking == 'b') /* file locking */ {
      struct flock sflo;
      int cmd = (locking == 'l' ? F_SETLK : F_SETLKW);

      sflo.l_whence = 0; sflo.l_start = 0; sflo.l_len = 0;
      sflo.l_type = 
        (modecodif[2] == 'r' || 
         (modecodif[2] =='\0' && modecodif[0] == 'r') ? F_RDLCK
         : F_WRLCK);
      if (fcntl(fileno(fileptr), cmd, &sflo) < 0) {
        fclose(fileptr);
        return FALSE;
      }
    }
  }

  return
    cunify(Arg, ptr_to_stream(Arg,new_stream(X(0),modespec,fileptr)),X(2));

 bombit:
  if (current_ferror_flag == atom_on)
    switch (what_happened) {
    case STRANGE_SOURCE_SINK:
      BUILTIN_ERROR(DOMAIN_ERROR(SOURCE_SINK), X(0), 1); break;
    case INEXISTENT_SOURCE_SINK:
      BUILTIN_ERROR(EXISTENCE_ERROR(SOURCE_SINK), X(0), 1); break;
    case CANNOT_OPEN:
      BUILTIN_ERROR(PERMISSION_ERROR(OPEN, SOURCE_SINK),X(0),1); break;
    case SYS_ERROR:
      BUILTIN_ERROR(SYSTEM_ERROR,X(0),1); break;
    case FINISHED_RESOURCES:
      BUILTIN_ERROR(RESOURCE_ERROR(R_UNDEFINED),X(0),1); break;
    default:
      return FALSE;
    } else return FALSE;
}



/* ----------------------------------------------------------------------- */

/* as Quintus closing a stream object referring to user_input,user_output */
/*   or user_error will succeed but cause no changes */

extern LOCK stream_list_l;

bool_t prolog_close(Arg)
     Argdecl;
{
  ERR__FUNCTOR("streams_basic:close", 1);
  stream_node_t *stream;

  stream = stream_to_ptr(X(0), 'x');
  if (stream==NULL) {
    BUILTIN_ERROR(DOMAIN_ERROR(STREAM_OR_ALIAS), X(0), 1);
  } else if (stream==Input_Stream_Ptr) {
    Input_Stream_Ptr = stream_user_input;
  } else if (stream==Output_Stream_Ptr) {
    Output_Stream_Ptr = stream_user_output;
  } else if (stream==Error_Stream_Ptr) {
    Error_Stream_Ptr = stream_user_error;
  }

  if ((stream!=stream_user_input) &&
      (stream!=stream_user_output) &&
      (stream!=stream_user_error))
    {
      if (stream->streammode != 's')        /* Not a socket -- has FILE * */
        fclose(stream->streamfile);  /* Releases file locks automatically */
      else
        close(GetInteger(stream->label));            /* Needs a lock here */


 /* We are twiggling with a shared structure: lock the access to it */

      Wait_Acquire_lock(stream_list_l);

      stream->label = ERRORTAG;
      stream->backward->forward = stream->forward;
      stream->forward->backward = stream->backward;

      /* now ensure that no choicepoints point at the stream */
      {
	CIAO_REGISTER node_t *B;
	tagged_t t1, t2;

	t1 = PointerToTerm(stream);
	t2 = PointerToTerm(stream->forward);
	
	for (B = w->node;
	     ChoiceYounger(B,Choice_Start);
	     B = ChoiceCharOffset(B,-B->next_alt->node_offset))
	  if (B->next_alt==address_nd_current_stream && B->term[3]==t1)
	    B->term[3] = t2;
      }

      checkdealloc((tagged_t *)stream,sizeof(stream_node_t));
      Release_lock(stream_list_l);
    }
  return TRUE;
}

/* '$unix_popen'(+Command, +Mode, -Stream) */
bool_t prolog_unix_popen(Arg)
     Argdecl;
{
  FILE *f;
  char *streammode;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  streammode = (X(1) == atom_read ? "r" : "w");

  if (!(f = popen(GetString(X(0)),streammode)))
	return FALSE;

  return cunify(Arg,ptr_to_stream(Arg,
                                  new_stream((tagged_t)0, streammode, f)), X(2));

}

/* ----------------------------------------------------------------------- */

bool_t prolog_pipe(Arg)
     Argdecl;
{
  ERR__FUNCTOR("streams_basic:pipe", 2);
  FILE *in;
  FILE *out;
  int fd[ 2 ] ;

  if ( pipe( fd ) ) goto bombit;
  if ( !(in  = fdopen( fd[ 0 ] , "r" )) ) goto bombit;
  if ( !(out = fdopen( fd[ 1 ] , "w" )) ) goto bombit;

  return (cunify(Arg,ptr_to_stream(Arg, new_stream((tagged_t)0, "r", in)), X(0)) &&
	  cunify(Arg,ptr_to_stream(Arg, new_stream((tagged_t)0, "w", out)), X(1)));

 bombit:
  BUILTIN_ERROR(RESOURCE_ERROR(R_UNDEFINED),X(0),1) ;
}

/* ----------------------------------------------------------------------- */

void ENG_perror(s)
     char *s;
{

/* #if defined(Win32) */
/* #  define sys_errlist _sys_errlist */
/* #endif */

#if !defined(LINUX) && !defined(Win32) && !defined(DARWIN) && !defined(BSD)
/*   extern char *sys_errlist[]; */
  extern int errno;
#endif

  /*ENG_PRINTF2(stream_user_error, "%s: %s\n", s, sys_errlist[errno]);*/
    ENG_PRINTF2(stream_user_error, "ERROR: %s: %s\n", s, strerror(errno)); 
}

/* ----------------------------------------------------------------------- */

/* ISO Behavior (MCL): current_input and current_output shall unify its
   argument with the current input (re. output) stream, _not_ stream_alias.
   This is a bit relaxed here: we allow also for stream aliases to be passed
   in and out without raising an exception.  Same goes for current_output */ 

bool_t prolog_current_input(Arg)
     Argdecl;
{
  ERR__FUNCTOR("streams_basic:current_input", 1);
  DEREF(X(0), X(0));

  if (is_var_or_alias_or_stream(Arg, X(0))) {
    return cunify(Arg, ptr_to_stream(Arg,Input_Stream_Ptr), X(0));
  } else {
    BUILTIN_ERROR(DOMAIN_ERROR(STREAM_OR_ALIAS), X(0), 1);
  }
}


bool_t prolog_set_input(Arg)
     Argdecl;
{
  ERR__FUNCTOR("streams_basic:set_input", 1);
  int errcode;
  stream_node_t *stream;

  DEREF(X(0),X(0));

  stream = stream_to_ptr_check(X(0), 'r', &errcode);
  if (stream==NULL) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  Input_Stream_Ptr = stream;
  return TRUE;
}

/* ----------------------------------------------------------------------- */

bool_t prolog_current_output(Arg)
     Argdecl;
{
  ERR__FUNCTOR("streams_basic:current_output", 1);
  DEREF(X(0), X(0));

  if (is_var_or_alias_or_stream(Arg, X(0))) {
    return cunify(Arg, ptr_to_stream(Arg,Output_Stream_Ptr), X(0));
  } else {
    BUILTIN_ERROR(DOMAIN_ERROR(STREAM_OR_ALIAS), X(0), 1);
  }
}


bool_t prolog_set_output(Arg)
     Argdecl;
{
  ERR__FUNCTOR("streams_basic:set_output", 1);
  int errcode;
  stream_node_t *stream;

  DEREF(X(0),X(0));
  stream = stream_to_ptr_check(X(0), 'w', &errcode);
  if (stream==NULL) {
    printf(" Returned null, errcode is %d\n", errcode);
    BUILTIN_ERROR(errcode, X(0), 1);
  }
  Output_Stream_Ptr = stream;
  return TRUE;
}

/* ----------------------------------------------------------------------- */

/* Replacing the stream aliases pointer */

/* replace_stream(StreamAlias, NewStream) */

bool_t prolog_replace_stream(Arg)
     Argdecl;
{
  ERR__FUNCTOR("io_alias_redirection:replace_stream", 2);
  tagged_t which_stream;
  tagged_t which_atom;
  stream_node_t *node;
  int errcode;

  DEREF(which_atom, X(0));
  DEREF(which_stream, X(1));

  if ((which_atom == atom_user_error) ||
      (which_atom == atom_user_output)) {
    node = stream_to_ptr_check(which_stream, 'w', &errcode);    
  } else if (which_atom == atom_user_input) {
    node = stream_to_ptr_check(which_stream, 'r', &errcode);    
  } else {
    /* Not exactly: should be "alias"*/
    BUILTIN_ERROR(DOMAIN_ERROR(STREAM_OR_ALIAS),X(0),1);
  }
  
  if (node == NULL) BUILTIN_ERROR(errcode,X(0),1);

  if (which_atom == atom_user_input) {
    stream_user_input = node;
  } else if (which_atom == atom_user_output) {
    stream_user_output = node;
  } else if (which_atom == atom_user_error) {
    stream_user_error = node;
  }

  return TRUE;
}


/* get_stream(StreamAlias, CurrentStream) */

bool_t prolog_get_stream(Arg)
     Argdecl;
{
  ERR__FUNCTOR("io_alias_redirection:get_stream", 2);
  tagged_t which_atom;
  stream_node_t *node;

  DEREF(which_atom, X(0));
  if (which_atom == atom_user_input) {
    node = stream_user_input;
  } else if (which_atom == atom_user_output) {
    node = stream_user_output;
  } else if (which_atom == atom_user_error) {
    node = stream_user_error;
  } else {
    BUILTIN_ERROR(DOMAIN_ERROR(STREAM_OR_ALIAS),X(0),1);
  }
   
  return cunify(Arg, X(1), ptr_to_stream_noalias(Arg, node));
}

/* ----------------------------------------------------------------------- */

bool_t prolog_current_error(Arg)
     Argdecl;
{
  return cunify(Arg,ptr_to_stream(Arg,Error_Stream_Ptr),X(0));
}

/* ----------------------------------------------------------------------- */

/* prolog_stream_code(?Stream,?Stream_code)
 * stream Stream	A prolog Stream
 * integer Stream_code  A unique number associated with Stream
 *
 * Description: Stream can be used by prolog predicates to perform IO
 * Stream_code can be used by C functions to somehow perform IO.
 * There is a problem due to the ambiguity of 'user':
 *	stream_code(user,X)
 * will return the stream code associated with user_output.
 */

bool_t prolog_stream_code(Arg)
     Argdecl;
{
  ERR__FUNCTOR("streams_basic:stream_code", 2);
  int errcode;
  stream_node_t *s;

  DEREF(X(1), X(1));
  if (IsVar(X(1)))
    {
      s = stream_to_ptr_check(X(0), 'x', &errcode);
      if (!s) {
        BUILTIN_ERROR(errcode,X(0),1);
      }

      if (s->streammode != 's'){                            /* Not a socket */
        Unify_constant(MakeSmall(fileno(s->streamfile)),X(1));
      } else {                                                  /* DCG, MCL */
        Unify_constant(s->label,X(1)); /* Can't be this done above as well? */
      }
      return TRUE;
    }
  else if (X(1) >= TaggedZero && X(1) < MakeSmall(ENG_NOFILES))
    {
      for (s = root_stream_ptr->backward;
	   s != root_stream_ptr && s->label != X(1);
	   s = s->backward)
	;
      if (s != root_stream_ptr && s->label == X(1))
	return cunify(Arg,ptr_to_stream(Arg,s),X(0));
      else
	return FALSE;
    }
  else if (IsInteger(X(1)))
    return FALSE;
  else
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(1),2);
}


bool_t character_count(Arg)
     Argdecl;
{
  ERR__FUNCTOR("streams_basic:character_count", 2);
  int errcode;
  stream_node_t *stream;

  stream = stream_to_ptr_check(X(0), 'x', &errcode);
  if (!stream) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  if (stream->isatty)
    stream = root_stream_ptr;
  return cunify(Arg,MakeInteger(Arg,stream->char_count),X(1));
}


bool_t line_position(Arg)
     Argdecl;
{
  ERR__FUNCTOR("streams_basic:line_position", 2);
  int errcode;
  stream_node_t *stream;

  stream = stream_to_ptr_check(X(0), 'x', &errcode);
  if (!stream) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  if (stream->isatty)
    stream = root_stream_ptr;
  return cunify(Arg,MakeInteger(Arg,stream->char_count-stream->last_nl_pos),X(1));
}


bool_t line_count(Arg)
     Argdecl;
{
  ERR__FUNCTOR("streams_basic:line_count", 2);
  int errcode;
  stream_node_t *stream;

  stream = stream_to_ptr_check(X(0), 'x', &errcode);
  if (!stream) {
    BUILTIN_ERROR(errcode,X(0),1);
  }

  if (stream->isatty)
    stream = root_stream_ptr;
  return cunify(Arg,MakeInteger(Arg,stream->nl_count),X(1));
}

/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       CURRENT_STREAM/3
   -----------------------------------------------------------------------*/

static bool_t current_stream_data(Arg,streamptr)
     Argdecl;
     stream_node_t *streamptr;
{
  Unify_constant(streamptr->streamname,X(0));
  switch (streamptr->streammode)
    {
    case 'a':
      Unify_constant(atom_append,X(1));
      break;
    case 'r':
      Unify_constant(atom_read,X(1));
      break;
    case 'w':
      Unify_constant(atom_write,X(1));
      break;
    case 's':
      Unify_constant(atom_socket,X(1));
      break;
    }
  return TRUE;
}


bool_t current_stream(Arg)
     Argdecl;
{
  stream_node_t *streamptr;

  DEREF(X(2),X(2));
  if (!IsVar(X(2)) && (streamptr=stream_to_ptr(X(2),'y')))
    return current_stream_data(Arg,streamptr);

  streamptr = root_stream_ptr->forward;
  while (streamptr!=root_stream_ptr &&
	 streamptr->streamname==ERRORTAG) /* skip over system streams */
    streamptr = streamptr->forward;
  if (streamptr==root_stream_ptr)
    return FALSE;
  else if (streamptr->forward!=root_stream_ptr)
    {
      X(3) = PointerToTerm(streamptr->forward);
      push_choicept(Arg,address_nd_current_stream);
    }

  return (cunify(Arg,ptr_to_stream(Arg,streamptr),X(2)) &&
	  current_stream_data(Arg,streamptr));
}

bool_t nd_current_stream(Arg)
     Argdecl;
{
  stream_node_t *streamptr = TagToStream(X(3));

  if (streamptr==root_stream_ptr)
    {				/* zero alts due to close */
      pop_choicept(Arg);
      return FALSE;
    }
  else if (streamptr->forward==root_stream_ptr)	/* last alt */
    pop_choicept(Arg);
  else
    w->node->term[3]=PointerToTerm(streamptr->forward);
  return (cunify(Arg,ptr_to_stream(Arg,streamptr),X(2)) &&
	  current_stream_data(Arg,streamptr));
}

/* ----------------------------------------------------------------------- */

bool_t prolog_bootversion(Arg)
     Argdecl;
{
  print_string(Output_Stream_Ptr, emulator_version);
  print_string(Output_Stream_Ptr, "\n");
  return TRUE;
}

/*
bool_t prolog_sourcepath(Arg)
     Argdecl;
{
  char cbuf[MAXPATHLEN];

  DEREF(X(0),X(0));
  strcpy(cbuf,source_path);
  strcat(cbuf,"/");
  strcat(cbuf,GetString(X(0)));
  Unify_constant(MakeString(cbuf),X(1));
  return TRUE;
}
*/

