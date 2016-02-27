/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#ifndef _SUPPORT_H
#define _SUPPORT_H

#include "debug.h"
#include "initial.h"
#include "threads.h"
#include "locks.h"

#define NULL_TRAIL_ENTRY MakeSmall(0)
#define IsCanceled(T) (T == NULL_TRAIL_ENTRY)
#define NullifyTrailEntry(P) *(P) = NULL_TRAIL_ENTRY

/*****************************************************************************/
/* Worker argument abstraction (from optim_comp) */

#define CHANGE_WORKER(NEW_WORKER, CODE) { \
  worker_t *old_w; \
  old_w = w; \
  SET_WORKER((NEW_WORKER)); \
  CODE; \
  SET_WORKER(old_w); \
}
#define LOCAL_WORKER(NEW_WORKER, CODE) { \
  worker_t *w; \
  SET_WORKER((NEW_WORKER)); \
  CODE; \
}

#if defined(USE_GLOBAL_WORKER)
/* todo: worker_t not yet defined? */
extern __thread worker_t *w;
#define DECL_WORKER_ARG
#define DECL_WORKER_ARGC
#define WORKER_ARG
#define WORKER_ARGC
#define WORKERATTR
/* note: in both macros, if CODE jumps somewhere else (goto, return or
   longjmp) then the old worker will not be restored  */
#define SET_WORKER(NEW_WORKER) w = (NEW_WORKER) 
/* note: use from CVOID CINSN CBOOL etc code */
#define CVOID__WITH_WORKER(NEW_WORKER, CODE) CHANGE_WORKER(NEW_WORKER, CODE) 
/* note: use from normal C functions */
#define WITH_WORKER(NEW_WORKER, CODE) CHANGE_WORKER(NEW_WORKER, CODE)
#else
#define DECL_WORKER_ARG worker_t *w
#define DECL_WORKER_ARGC DECL_WORKER_ARG,
#define WORKER_ARG w
#define WORKER_ARGC WORKER_ARG,
#if defined(USE_REGWORKERARG)
/* todo: does not work in gcc prior to 3.4.0 (CAST) */
#define WORKERATTR __attribute__ ((regparm(1)))
#else
#define WORKERATTR
#endif
#define SET_WORKER(NEW_WORKER) w = (NEW_WORKER)
/* note: in both macros, if CODE jumps somewhere else (goto, return or
   longjmp) then the old worker will not be restored  */
/* note: use from CVOID CINSN CBOOL etc code */
#define CVOID__WITH_WORKER(NEW_WORKER, CODE) CHANGE_WORKER(NEW_WORKER, CODE)
/* note: use from normal C functions */
#define WITH_WORKER(NEW_WORKER, CODE) LOCAL_WORKER(NEW_WORKER, CODE)
#endif

/*****************************************************************************/
/* Calling protocols (from optim_comp) */

/*
  The C code that implements predicates, builtins or complex abstract
  machine operations can be called following several protocols.

  - CFUN: a deterministic predicate with one output term
  - CBOOL: semideterministic predicate returns the success state
  - CINSNP: returns the continuation pointer (for compilation to C)
  - CVOID: deterministic predicate
  
  --JFMC
*/


#define CINSNP__PROTO(NAME) bcp_t WORKERATTR NAME(DECL_WORKER_ARG)
#define CINSNP__PROTO_N(NAME, ...) bcp_t WORKERATTR NAME(DECL_WORKER_ARGC __VA_ARGS__)
#define CINSNP__INSNCONT(NAME) NAME(WORKER_ARG)
#define CINSNP__INSNCONT_N(NAME, ...) NAME(WORKER_ARGC __VA_ARGS__)
#define CINSNP__LASTCALL(NAME) return NAME(WORKER_ARG)
#define CINSNP__LASTCALL_N(NAME, ...) return NAME(WORKER_ARGC __VA_ARGS__)
#define CINSNP__GOTO(NAME) return NAME
#define CINSNP__PROCEED return SUCCESS_INSNP
#define CINSNP__FAIL return FAIL_INSNP

#define CFUN__PROTO(NAME, TYPE) TYPE WORKERATTR NAME(DECL_WORKER_ARG)
#define CFUN__PROTO_N(NAME, TYPE, ...) TYPE WORKERATTR NAME(DECL_WORKER_ARGC __VA_ARGS__)
#define CFUN__EVAL(NAME) NAME(WORKER_ARG)
#define CFUN__EVAL_N(NAME, ...) NAME(WORKER_ARGC __VA_ARGS__)
#define CFUN__LASTCALL(NAME) return NAME(WORKER_ARG)
#define CFUN__LASTCALL_N(NAME, ...) return NAME(WORKER_ARGC __VA_ARGS__)
#define CFUN__PROCEED(X) return (X)

#define CBOOL__PROTO(NAME) bool_t WORKERATTR NAME(DECL_WORKER_ARG)
#define CBOOL__PROTO_N(NAME, ...) bool_t WORKERATTR NAME(DECL_WORKER_ARGC __VA_ARGS__)
#define CBOOL__SUCCEED(NAME) NAME(WORKER_ARG)
#define CBOOL__SUCCEED_N(NAME, ...) NAME(WORKER_ARGC __VA_ARGS__)
#define CBOOL__CALL(NAME) if (!NAME(WORKER_ARG)) return FALSE
#define CBOOL__CALL_N(NAME, ...) if (!NAME(WORKER_ARGC __VA_ARGS__)) return FALSE
#define CBOOL__LASTCALL(NAME) return NAME(WORKER_ARG)
#define CBOOL__LASTCALL_N(NAME, ...) return NAME(WORKER_ARGC __VA_ARGS__)
#define CBOOL__TEST(X) if (!(X)) return FALSE
#define CBOOL__LASTTEST(X) return (X)
#define CBOOL__PROCEED return TRUE
#define CBOOL__FAIL return FALSE

#define CVOID__PROTO(NAME) void WORKERATTR NAME(DECL_WORKER_ARG)
#define CVOID__PROTO_N(NAME, ...) void WORKERATTR NAME(DECL_WORKER_ARGC __VA_ARGS__)
#define CVOID__CALL(NAME) NAME(WORKER_ARG)
#define CVOID__CALL_N(NAME, ...) NAME(WORKER_ARGC __VA_ARGS__)
#define CVOID__PROCEED return

/*****************************************************************************/

/*  Macros for BUILTIN C-PREDICATE support  */

extern bcp_t bootcode;
#if defined(INTERNAL_CALLING)
extern bcp_t internal_calling;
#endif
extern bcp_t startgoalcode;
extern bcp_t startgoalcode_cont;
extern bcp_t contcode;
extern bcp_t failcode;
extern bcp_t exitcode;
extern try_node_t *termcode;
extern try_node_t *fail_alt;
extern bool_t predtrace;
extern ENG_INT mem_prog_count;
extern instance_clock_t def_clock, use_clock;
extern sw_on_key_t *switch_on_function;
extern definition_t *int_address;
/*extern char *emulator_path;*/ /* Unused now.  DCG. */
extern char *emulator_version;
extern char incremental_symbol_table_path[];
/*extern tagged_t *numstack_end;*/
/* extern char *mem_start; */ /* Unused now.  MCL. */

/* These are for error handling (DCG) */
/* Changed to be private for every thread */
/*
extern int ErrArgNo;
extern tagged_t Culprit;
*/

extern bool_t cunify PROTO((worker_t *w, tagged_t u, tagged_t v));
extern tagged_t make_integer_check PROTO((Argdecl, ENG_INT i, bcp_t op));
extern tagged_t make_float_check PROTO((Argdecl, ENG_FLT i, bcp_t op));
extern tagged_t make_structure PROTO((Argdecl, tagged_t functor));
extern tagged_t init_atom_check PROTO((char *str));
extern tagged_t evaluate PROTO((Argdecl, tagged_t v));

extern char * tryalloc_errstring;
extern tagged_t *tryalloc PROTO((int size));
extern tagged_t *tryrealloc PROTO((tagged_t *ptr, int decr, int size));
extern tagged_t *checkalloc PROTO((int size));
extern tagged_t *checkrealloc PROTO((tagged_t *ptr, int decr, int size));

extern void add_definition PROTO((sw_on_key_t **swp, sw_on_key_node_t *node, tagged_t key, definition_t *def));
extern definition_t *insert_definition PROTO((sw_on_key_t **swp, tagged_t tagpname, int arity, bool_t insertp));
extern definition_t *find_definition PROTO((sw_on_key_t **swp, tagged_t term, tagged_t **argl, bool_t insertp));
extern definition_t *parse_definition PROTO((tagged_t complex));

extern stream_node_t *new_stream PROTO((tagged_t name, char *mode, FILE *file));
extern stream_node_t *stream_to_ptr PROTO((tagged_t t, int mode));
extern stream_node_t *stream_to_ptr_check PROTO((tagged_t t, int mode, int *errcode));
extern sw_on_key_t *new_switch_on_key PROTO((int size, try_node_t *otherwise));
//extern try_node_t *def_retry_c PROTO((bool_t (*proc)(worker_t *w), int arity));
extern try_node_t *def_retry_c PROTO((bool_t (*proc)(), int arity));
extern sw_on_key_node_t *incore_gethash PROTO((sw_on_key_t *sw, tagged_t k));
extern instance_t *current_instance PROTO((worker_t *w));
extern bool_t next_instance PROTO((worker_t *w, instance_t **ipp));
extern bool_t next_instance_conc PROTO((worker_t *w, instance_t **ipp));
extern sw_on_key_node_t *dyn_puthash PROTO((sw_on_key_t **swp, tagged_t k));
extern void leave_to_gc PROTO((int type, char *ptr));
extern void updateChoicepoints PROTO((int decrement));
extern void compressTrail PROTO((Argdecl, bool_t from_gc));
extern void print_string PROTO((stream_node_t *stream, char *p));
extern void print_variable PROTO((worker_t *w, stream_node_t *stream, tagged_t term));
extern void print_number PROTO((Argdecl, stream_node_t *stream, tagged_t term));
/*extern void print_atom PROTO((stream_node_t *stream, tagged_t term));*/
extern int compile_large PROTO((tagged_t term, bcp_t p));
extern instance_t *compile_term_aux PROTO((worker_t *w, tagged_t head, tagged_t body, worker_t **w_n));

extern ENG_INT get_integer PROTO((tagged_t t));
extern ENG_FLT get_float PROTO((tagged_t t));
extern bool_t float_is_finite PROTO((tagged_t t));
extern tagged_t make_large PROTO((Argdecl,tagged_t *p));
extern tagged_t make_integer PROTO((Argdecl,ENG_INT i));
extern tagged_t make_float PROTO((Argdecl,ENG_FLT f));

extern worker_t *create_wam_storage PROTO((void));

#define DerefHeap(Xderef,Ptr) \
{ \
  CIAO_REG_Z(tagged_t, m_i); \
  CIAO_REG_Z(tagged_t, m_j); \
 \
  RefHeap(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefCar(Xderef,Ptr) \
{ \
  CIAO_REG_Z(tagged_t, m_i); \
  CIAO_REG_Z(tagged_t, m_j); \
 \
  RefCar(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefCdr(Xderef,Ptr) \
{ \
  CIAO_REG_Z(tagged_t, m_i); \
  CIAO_REG_Z(tagged_t, m_j); \
 \
  RefCdr(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefArg(Xderef,Ptr,I) \
{ \
  CIAO_REG_Z(tagged_t, m_i); \
  CIAO_REG_Z(tagged_t, m_j); \
 \
  RefArg(m_i,Ptr,I); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefHeapNext(Xderef,Ptr) \
{ \
  CIAO_REG_Z(tagged_t, m_i); \
  CIAO_REG_Z(tagged_t, m_j); \
 \
  RefHeapNext(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}


#define DEREF(Xderef,X) \
{ \
  CIAO_REG_Z(tagged_t, m_i); \
  CIAO_REG_Z(tagged_t, m_j); \
 \
  m_i = X; \
  DerefSwitch(m_i,m_j,;) \
  Xderef = m_i; \
}

#define SwitchOnVar(Reg,Aux,HVACode,CVACode,SVACode,NVACode) \
{ \
    for (;;) \
      { \
	  if (!IsVar(Reg)) \
	    NVACode \
	  else if (Reg & TagBitSVA) \
	    { RefSVA(Aux,Reg); \
	      if (Reg!=Aux) { Reg=Aux; continue; } \
	      else SVACode \
	    } \
	  else if (!(Reg & TagBitCVA)) \
	    { RefHVA(Aux,Reg); \
	      if (Reg!=Aux) { Reg=Aux; continue; } \
	      else HVACode \
	    } \
	  else \
	    { RefCVA(Aux,Reg); \
	      if (Reg!=Aux) { Reg=Aux; continue; } \
	      else CVACode \
	    } \
	  break; \
	} \
}

#define SwitchOnHeapVar(Reg,Aux,HVACode,CVACode,NVACode) \
{ \
    for (;;) \
      { \
	  if (!IsVar(Reg)) \
	    NVACode \
	  else if (!(Reg & TagBitCVA)) \
	    { RefHVA(Aux,Reg); \
	      if (Reg!=Aux) { Reg=Aux; continue; } \
	      else HVACode \
	    } \
	  else \
	    { RefCVA(Aux,Reg); \
	      if (Reg!=Aux) { Reg=Aux; continue; } \
	      else CVACode \
	    } \
	  break; \
	} \
}



#define DerefSwitch(Reg,Aux,VarCode) \
{ \
  if (IsVar(Reg)) \
    do \
      if (Reg == (Aux = CTagToPointer(Reg))) \
	{VarCode;break;} \
    while (IsVar(Reg=Aux)); \
}

#define DerefHeapSwitch(Reg,Aux,VarCode) DerefSwitch(Reg,Aux,VarCode)


#define YoungerHeapVar(Q,R)	HeapYounger(Q,R)
#define YoungerStackVar(Q,R)	StackYounger(Q,R)

#if defined(PARBACK) || defined(ANDPARALLEL)
#define CondHVA(X)		(!OffHeaptop(X,w->global_uncond) || !OnHeap(TagToPointer(X)))
#define CondCVA(X)		(!OffHeaptop(TagHVA(TagToCVA(X)),w->global_uncond) || !OnHeap(TagToPointer(X)))
#define CondSVA(X)		(!OffStacktop(X,w->local_uncond) || !OnStack(TagToPointer(X)))
#else
#define CondHVA(X)		(!OffHeaptop(X,w->global_uncond))
#define CondCVA(X)		(!OffHeaptop(TagHVA(TagToCVA(X)),w->global_uncond))
#define CondSVA(X)		(!OffStacktop(X,w->local_uncond))
#endif
#define CondStackvar(X)		CondSVA(X)

#define BindingOfHVA(X)		CTagToHVA(X)
#define BindingOfCVA(X)		CTagToCVA(X)
#define BindingOfSVA(X)		CTagToSVA(X)
#define BindingOfStackvar(X)	X

/* segfault patch -- jf */
void trail_push_check(Argdecl, tagged_t x);

/* segfault patch -- jf */
#define BindCVA(U,V) \
{ \
  TrailPushCheck(w->trail_top,U); \
  CTagToCVA(U) = V; \
}

#define BindSVA(U,V)				\
  {						\
    if (CondSVA(U))				\
      TrailPushCheck(w->trail_top,U);		\
    CTagToSVA(U) = V;				\
  }

#define BindHVA(U,V)				\
  {						\
    if (CondHVA(U))				\
      TrailPushCheck(w->trail_top,U);		\
    CTagToHVA(U) = V;				\
  }

#define Wake \
{ \
  SetEvent, Heap_Warn_Soft = HeapCharOffset(Heap_Warn_Soft,-1); \
}

#define WakeCount (TestEvent ? HeapCharDifference(Heap_Warn_Soft,Heap_Start) : 0)

//TODO: nullify fake trail entries with a predicate which makes nothing.
#define PlainUntrail(TR,Ref,CONT)					\
  {									\
    Ref = TrailPop(TR);							\
    if (!IsVar(Ref))							\
      {if (!IsCanceled(Ref)) CONT}					\
    else								\
      CTagToPointer(Ref) = Ref;						\
  } 

/* SERIOUS_FAULT - a fault that should not occur- indicating a corruption
                  such as following the STR tag not coming to a FNT tag
		  this kind of fault may not need to be testing in final
		  version but must in testing cause a total abort
   USAGE_FAULT   - a fault in the usage(incorrect parameters) of a 
                  builtin predicate - an error message is written.
   MINOR_FAULT   - a fault that should result in a error message being
                  written somewhere, but the builtin predicate just
		  fails and is not aborted
*/


/* Exit code from wam() when aborting */
#define WAM_ABORT -32768 /* see exceptions.pl */
#define WAM_INTERRUPTED -32767

#include <setjmp.h>
#include "compat.h"

extern JMP_BUF abort_env;

extern void failc(char *mesg);

#define EXIT(Y)           {ENG_PRINTF1(stream_user_error,"{ERROR: %s}\n", Y); \
                           fflush(NULL); \
                           at_exit(-1);}

#define _EXIT(Y)          {ENG_PRINTF1(stream_user_error,"{ERROR: %s}\n", Y); \
                           fflush(NULL); \
                           _exit(-1);}

#define SERIOUS_FAULT(Y)       {failc(Y); \
                                LONGJMP(abort_env, WAM_ABORT); }
                          
#define MAJOR_FAULT(Y)         {failc(Y); return FALSE;}

#define USAGE_FAULT(Y)         {failc(Y); return FALSE;}

#define MINOR_FAULT(Y)         {return FALSE;}


/* Error codes, xref errhandle.pl, internals.pl //) */
/* OGRAMA: error classification ISO PROLOG */


/* For any change / addition to this list:
   
            PLEASE DO READ AND UPDATE THE CORRESPONDING FACTS IN
                        ciao/lib/engine/internals.pl

    Error messages are given by ciao/lib/errhandle.pl .  Learn how these
    work before updating anything here.

*/


/* Errors identifiers cannot be zero (as 0 = -0) */
#define INSTANTIATION_ERROR     1
#define TYPE_ERROR(D)           (RANGE_PER_ERROR*START_TYPE+D)
#define DOMAIN_ERROR(D)         (RANGE_PER_ERROR*START_DOM+D)
#define EXISTENCE_ERROR(D)      (RANGE_PER_ERROR*START_EXIST+D)
#define PERMISSION_ERROR(D,F)   (RANGE_PER_ERROR*START_PERM+D*10+F)
#define REPRESENTATION_ERROR(D) (RANGE_PER_ERROR*START_REPRES+D)
#define EVALUATION_ERROR(D)     (RANGE_PER_ERROR*START_EVAL+D)
#define RESOURCE_ERROR(D)       (RANGE_PER_ERROR*START_RES+D)
#define SYNTAX_ERROR            (RANGE_PER_ERROR*START_SYNTAX)
#define SYSTEM_ERROR            (RANGE_PER_ERROR*START_SYSTEM)
#define USER_EXCEPTION          (RANGE_PER_ERROR*START_USER)

#define RANGE_PER_ERROR 100                    /* Enough number of errors */

#define START_INST    0
#define START_TYPE    1
#define START_DOM     2
#define START_EXIST   3
#define START_PERM    4
#define START_REPRES  5
#define START_EVAL    6
#define START_RES     7
#define START_SYNTAX  8
#define START_SYSTEM  9
#define START_USER    10

/* TYPE_ERRORS */
#define STRICT_ATOM          0
#define ATOMIC               1
#define BYTE                 2
#define CHARACTER            3
#define COMPOUND             4
#define EVALUABLE            5
#define IN_BYTE              6
#define INTEGER              7
#define LIST                 8
#define NUMBER               9
#define PREDICATE_INDICATOR 10
#define VARIABLE            11
#define CALLABLE            12

/* DOMAIN_ERRORS */
#define CHARACTER_CODE_LIST     0
#define SOURCE_SINK             1
#define STREAM                  2
#define IO_MODE                 3
#define NON_EMPTY_LIST          4
#define NOT_LESS_THAN_ZERO      5
#define OPERATOR_PRIORITY       6
#define PROLOG_FLAG             7
#define READ_OPTION             8
#define FLAG_VALUE              9
#define CLOSE_OPTION           10
#define STREAM_OPTION          11
#define STREAM_OR_ALIAS        12
#define STREAM_POSITION        13
#define STREAM_PROPERTY        14
#define WRITE_OPTION           15
#define OPERATOR_SPECIFIER     16


/* EXISTENCE_ERRORS */
#define PROCEDURE 0
/* SOURCE_SINK and STREAM already defined */
/*
#define SOURCE_SINK             1
#define STREAM                  2
*/

/* PERMISION_ERRORS: composed of type of action + object on which the action
   is defined */

/* PERMISSION_TYPE */
#define ACCESS      0
#define CREATE      1
#define INPUT       2
#define MODIFY      3
#define OPEN        4
#define OUTPUT      5
#define REPOSITION  6

/* OBJECTS */
#define BINARY_STREAM        0
/*
#define SOURCE_SINK             1
#define STREAM                  2
*/
#define TEXT_STREAM          3
#define FLAG                 4
#define OPERATOR             5
#define PAST_END_OF_STREAM   6
#define PRIVATE_PROCEDURE    7
#define STATIC_PROCEDURE     8



/* REPRESENTATION_ERROR */

/* CHARACTER_CODE_LIST already defined */
/* #define CHARACTER_CODE_LIST     0 */
#define IN_CHARACTER_CODE     1
#define MAX_ARITY             2
/*#define CHARACTER            3*/
#define MAX_INTEGER           4
#define MIN_INTEGER           5
#define CHARACTER_CODE        6
#define NAN_OR_INF_TO_INTEGER 7

/* EVALUATION_ERROR */
#define FLOAT_OVERFLOW 0
#define INT_OVERFLOW   1
#define E_UNDEFINED    2
#define E_UNDERFLOW    3
#define ZERO_DIVISOR   4

/* RESOURCE_ERROR */
#define R_UNDEFINED    0
#define R_STACK        1



/* OGRAMA: OLD VERSION ---------------------------------------------------- */ 
/* #define TYPE_ERROR(Type) (32+Type) */ /* includes also domain errors */ 

/* #define INSTANTIATION_ERROR 1
#define READ_PAST_EOS_ERROR 2
#define NO_READ_PERMISSION 3
#define NO_WRITE_PERMISSION 4
#define NO_SUCH_FILE 5
#define NO_OPEN_PERMISSION 6
#define NO_ACCESS_PERMISSION 7
#define SYSTEM_ERROR 8 OGRAMA: Error de sistema */

/* Type codes for TYPE_ERROR  //) */
/* #define STRICT_ATOM 0
#define ATOMIC 1
#define BYTE 2
#define CALLABLE 3
#define COMPOUND 4
#define EVALUABLE 5
#define IN_BYTE 6
#define INTEGER 7
#define LIST 8
#define NUMBER 9
#define PREDICATE_INDICATOR 10
#define VARIABLE 11 
*/

/*
#define CHARACTER_CODE_LIST 32 First domain code 
#define STREAM_OR_ALIAS 33
#define SOURCE_SINK 34  OGRAMA */
/* END OLD VERSION ----------------------------------------------------------- */


#if defined(USE_OC_EXCEPTIONS) /* Code from optim_comp */

/* Exceptions */
/* todo: pass worker as argument to macros? */

/* usage: goto, continue, break is forbidden inside CODE! */
#define EXCEPTION__CATCH(CODE, HANDLER) ({ \
  JMP_BUF catch_exception__handler; \
  JMP_BUF *catch_exception__old_handler; \
  catch_exception__old_handler = w->misc->errhandler; \
  w->misc->errhandler = &catch_exception__handler; \
  if (SETJMP(catch_exception__handler)) { \
    /* just in case of a worker expansion */ \
    w = desc->worker_registers; \
    w->misc->errhandler = catch_exception__old_handler; \
    HANDLER; \
  } else { \
    CODE; \
  } \
})

#define UNLOCATED_EXCEPTION(Code) {		\
    ErrCode = Code;				\
    ErrFuncName = "unknown";			\
    ErrFuncArity = -1;				\
    ErrArgNo = 0;				\
    Culprit = TaggedZero;			\
    EXCEPTION__THROW;				\
  }


#define EXCEPTION__THROW LONGJMP(*w->misc->errhandler, 1)

/* Throwing exceptions from builtins */

#define ERR__FUNCTOR(NAME, ARITY) \
  static char *const err__name = NAME; static const int err__arity = ARITY;

#define BUILTIN_ERROR(Code,Culpr,ArgNo) ({ \
  ErrCode = Code; \
  ErrFuncName = err__name; \
  ErrFuncArity = err__arity; \
  ErrArgNo = ArgNo; Culprit = Culpr; \
  EXCEPTION__THROW; \
})

#define ERROR_IN_ARG(Arg,ArgNo,ReqType) ({ \
  BUILTIN_ERROR(IsVar(Arg) ? INSTANTIATION_ERROR : TYPE_ERROR(ReqType), \
                Arg, ArgNo); \
})

/* todo: both versions are the same here! */
/* NDEREF from a wambuiltin */
#define NDEREF_B(Wam, Reg, ArgNo, Aux)					\
  {									\
    DerefSwitch(Reg,Aux,						\
		BUILTIN_ERROR(INSTANTIATION_ERROR, (Reg), (ArgNo));)	\
      if (!IsNumber(Reg))						\
	{								\
	  Reg = evaluate(Wam, Reg);					\
	  if(!IsNumber(Reg))						\
	    BUILTIN_ERROR(TYPE_ERROR(EVALUABLE), (Reg), (ArgNo));	\
	}								\
  }
/* NDEREF from a wamfunction */
#define NDEREF_F(Wam, Reg, ArgNo, Aux)					\
  {									\
    DerefSwitch(Reg,Aux,						\
		BUILTIN_ERROR(INSTANTIATION_ERROR, (Reg), (ArgNo));)	\
    if (!IsNumber(Reg))							\
	{								\
	  Reg = evaluate(Wam, Reg);					\
	  if(!IsNumber(Reg))						\
	    BUILTIN_ERROR(TYPE_ERROR(EVALUABLE), (Reg), (ArgNo));	\
	}								\
  }
/* NDEREF_I */
#define NDEREF_I(Wam, Reg, ArgNo, Aux)					\
  { 									\
    DerefSwitch(Reg,Aux,						\
		BUILTIN_ERROR(INSTANTIATION_ERROR, (Reg), (ArgNo));)	\
      if (!IsInteger(Reg))						\
	{								\
	  Reg = evaluate(Wam, Reg);					\
	  if(!TagIsSmall(Reg)) {					\
	    if(!TagIsLarge(Reg))					\
	      BUILTIN_ERROR(TYPE_ERROR(EVALUABLE), (Reg), (ArgNo));	\
	    if(LargeIsFloat(Reg))					\
	      BUILTIN_ERROR(TYPE_ERROR(INTEGER), (Reg), (ArgNo));	\
	  }								\
	}								\
  }

#else /* Default exceptions */

#define ERR__FUNCTOR(NAME, ARITY) 

#define BUILTIN_ERROR(Code,Culpr,ArgNo) \
  { ErrArgNo = ArgNo; Culprit = Culpr; return -Code; }

#define ERROR_IN_ARG(Arg,ArgNo,ReqType) \
{ ErrArgNo = ArgNo; Culprit = Arg; \
  return (IsVar(Arg) ? -INSTANTIATION_ERROR : -TYPE_ERROR(ReqType)); \
}

/* NDEREF from a wambuiltin */
#define NDEREF_B(Wam, Reg, ArgNo, Aux) \
{ \
  DerefSwitch(Reg,Aux,{failc(illexp); return FALSE;}) \
  if (!IsNumber(Reg)) \
    { \
      Reg = evaluate(Wam, Reg); \
      if(!IsNumber(Reg)) \
        {failc(illexp); return FALSE;} \
    } \
}
/* NDEREF from a wamfunction */
#define NDEREF_F(Wam, Reg, ArgNo, Aux) \
{ \
  DerefSwitch(Reg,Aux,{failc(illexp); return ERRORTAG;}) \
  if (!IsNumber(Reg)) \
    { \
      Reg = evaluate(Wam, Reg); \
      if(!IsNumber(Reg)) \
        {failc(illexp); return ERRORTAG;} \
    } \
}

#define NDEREF_I(Wam, Reg, ArgNo, Aux) NDEREF_F(Wam, Reg, ArgNo, Aux)

#endif

/* MakeLST(To,Car,Cdr):
   
   Set 'To' to a term tagged_t LST
   whose car and cdr are 'Car' and Cdr'.

   'To' may be identical to 'Car' or 'Cdr'.
*/
#define MakeLST(To,Car,Cdr) \
{ tagged_t makelst_car = (Car); \
  HeapPush(w->global_top,makelst_car); \
  HeapPush(w->global_top,Cdr); \
  To = Tag(LST,HeapOffset(w->global_top,-2)); \
}

/* MakeSTR(To,Functor):
   
   Set 'To' to a term tagged_t STR
   whose principal functor is 'Functor'.  
   Space is allocated for the arguments, but they are not filled in.
*/
#define MakeSTR(To,Functor) \
{ \
  HeapPush(w->global_top,Functor); \
  To = Tag(STR,HeapOffset(w->global_top,-1)); \
  w->global_top = HeapOffset(w->global_top,Arity(Functor)); \
}

#define Unify_constant(U,V) \
{ CIAO_REGISTER tagged_t m_t0, m_u=U, m_t1=V; \
  SwitchOnVar(m_t1,m_t0,{BindHVA(m_t1,m_u);}, \
	            {BindCVA(m_t1,m_u);Wake;}, \
	            {BindSVA(m_t1,m_u);}, \
		    {if (m_t1!=m_u) return FALSE;}) \
}


#define ENG_PRINTF1(S,FMT,A1) \
{ char m_buf[2048]; sprintf(m_buf,FMT,A1); print_string(S, m_buf); }

#define ENG_PRINTF2(S,FMT,A1,A2) \
{ char m_buf[2048]; sprintf(m_buf,FMT,A1,A2); print_string(S, m_buf); }

#define ENG_PRINTF3(S,FMT,A1,A2,A3) \
{ char m_buf[2048]; sprintf(m_buf,FMT,A1,A2,A3); print_string(S, m_buf); }

#define ENG_PRINTF4(S,FMT,A1,A2,A3,A4) \
{ char m_buf[2048]; sprintf(m_buf,FMT,A1,A2,A3,A4); print_string(S, m_buf); }

#define ENG_PRINTF5(S,FMT,A1,A2,A3,A4,A5) \
{ char m_buf[2048]; sprintf(m_buf,FMT,A1,A2,A3,A4,A5); print_string(S, m_buf); }

#define ENG_TTYPRINTF0(FMT) print_string(Error_Stream_Ptr,FMT) 

#define ENG_TTYPRINTF1(FMT,A1) ENG_PRINTF1(Error_Stream_Ptr,FMT,A1) 

#define ENG_TTYPRINTF2(FMT,A1,A2) ENG_PRINTF2(Error_Stream_Ptr,FMT,A1,A2) 

#define ENG_TTYPRINTF3(FMT,A1,A2,A3) ENG_PRINTF3(Error_Stream_Ptr,FMT,A1,A2,A3) 

#define ENG_TTYPRINTF4(FMT,A1,A2,A3,A4) ENG_PRINTF4(Error_Stream_Ptr,FMT,A1,A2,A3,A4) 


#define EXPAND_ATOM_BUFFER(new_max_atom_length) \
{ \
     Atom_Buffer = \
        (char *)checkrealloc((tagged_t *)Atom_Buffer, \
                             Atom_Buffer_Length, \
                             new_max_atom_length); \
    Atom_Buffer_Length = new_max_atom_length; \
}


extern ENG_INT (*eng_goal_from_thread_id)(THREAD_ID id);

#endif /* _SUPPORT_H */
