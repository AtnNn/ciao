/* Copyright (C) 1996,1997,1998, UPM-CLIP */

#include "debug.h"
#include "initial.h"

/*  Macros for BUILTIN C-PREDICATE support  */

extern INSN *bootcode;
#if defined(INTERNAL_CALLING)
extern INSN *internal_calling;
#endif
extern INSN *startgoalcode;
extern INSN *startgoalcode_cont;
extern INSN *contcode;
extern INSN *failcode;
extern INSN *exitcode;
extern struct try_node *termcode;
extern struct try_node *fail_alt;
extern BOOL predtrace;
extern BOOL prof_include_time;
extern ENG_INT mem_prog_count;
extern CLOCK def_clock, use_clock;
extern struct sw_on_key *switch_on_function;
extern struct definition *int_address;
/*extern char *emulator_path;*/ /* Unused now.  DCG. */
extern char *emulator_version;
extern char incremental_symbol_table_path[];
/*extern TAGGED *numstack_end;*/
/* extern char *mem_start; */ /* Unused now.  MCL. */

/* These are for error handling (DCG) */
/* Changed to be private for every thread */
/*
extern int ErrArgNo;
extern TAGGED Culprit;
*/

extern BOOL cunify PROTO((struct worker *w, TAGGED u, TAGGED v));
extern TAGGED ptr_to_stream PROTO((struct worker *w, struct stream_node *n));
extern TAGGED make_integer_check PROTO((Argdecl, ENG_INT i, INSN *op));
extern TAGGED make_float_check PROTO((Argdecl, ENG_FLT i, INSN *op));
extern TAGGED make_structure PROTO((Argdecl, TAGGED functor));
extern TAGGED init_atom_check PROTO((char *str));
extern TAGGED evaluate PROTO((Argdecl, TAGGED v));
extern TAGGED *checkalloc PROTO((int size));
extern TAGGED *checkrealloc PROTO((TAGGED *ptr, int decr, int size));

extern void add_definition PROTO((struct sw_on_key **swp, struct sw_on_key_node *node, TAGGED key, struct definition *def));
extern struct definition *insert_definition PROTO((struct sw_on_key **swp, TAGGED tagpname, int arity, BOOL insertp));
extern struct definition *find_definition PROTO((struct sw_on_key **swp, TAGGED term, TAGGED **argl, BOOL insertp));
extern struct definition *parse_definition PROTO((TAGGED complex));

extern struct stream_node *new_stream PROTO((TAGGED name, char *mode, FILE *file));
extern struct stream_node *stream_to_ptr PROTO((TAGGED t, int mode));
extern struct stream_node *stream_to_ptr_check PROTO((TAGGED t, int mode, int *errcode));
extern struct sw_on_key *new_switch_on_key PROTO((int size, struct try_node *otherwise));
extern struct try_node *def_retry_c PROTO((BOOL (*proc)(), int arity));
extern struct sw_on_key_node *incore_gethash PROTO((struct sw_on_key *sw, TAGGED k));
extern struct instance *current_instance PROTO((struct worker *w));
extern BOOL next_instance PROTO((struct worker *w, struct instance **ipp));
extern BOOL next_instance_conc PROTO((struct worker *w, struct instance **ipp));
extern struct sw_on_key_node *dyn_puthash PROTO((struct sw_on_key **swp, TAGGED k));
extern struct atom *new_atom_check PROTO((unsigned char *str, unsigned int hcode));
extern void leave_to_gc PROTO((int type, char *ptr));
extern void updateChoicepoints PROTO((int decrement));
extern void compressTrail PROTO((Argdecl, BOOL from_gc));
extern void print_string PROTO((struct stream_node *stream, char *p));
extern void print_variable PROTO((struct worker *w, struct stream_node *stream, TAGGED term));
extern void print_number PROTO((Argdecl, struct stream_node *stream, TAGGED term));
extern void print_atom PROTO((struct stream_node *stream, TAGGED term));
extern int compile_large PROTO((TAGGED term, INSN *p));
extern struct instance *compile_term_aux PROTO((struct worker *w, TAGGED head, TAGGED body, struct worker **w_n));

extern ENG_INT get_integer PROTO((TAGGED t));
extern ENG_FLT get_float PROTO((TAGGED t));
extern TAGGED make_large PROTO((Argdecl,TAGGED *p));
extern TAGGED make_integer PROTO((Argdecl,ENG_INT i));
extern TAGGED make_float PROTO((Argdecl,ENG_FLT f));

extern struct worker *create_wam_storage PROTO((void));

#define DerefHeap(Xderef,Ptr) \
{ \
  REGISTER TAGGED m_i, m_j; \
 \
  RefHeap(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefCar(Xderef,Ptr) \
{ \
  REGISTER TAGGED m_i, m_j; \
 \
  RefCar(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefCdr(Xderef,Ptr) \
{ \
  REGISTER TAGGED m_i, m_j; \
 \
  RefCdr(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefArg(Xderef,Ptr,I) \
{ \
  REGISTER TAGGED m_i, m_j; \
 \
  RefArg(m_i,Ptr,I); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefHeapNext(Xderef,Ptr) \
{ \
  REGISTER TAGGED m_i, m_j; \
 \
  RefHeapNext(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}


#define DEREF(Xderef,X) \
{ \
  REGISTER TAGGED m_i, m_j; \
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

#define CondHVA(X)		(!OffHeaptop(X,w->global_uncond))
#define CondCVA(X)		(!OffHeaptop(TagHVA(TagToCVA(X)),w->global_uncond))
#define CondSVA(X)		(!OffStacktop(X,w->local_uncond))
#define CondStackvar(X)		CondSVA(X)

#define BindingOfHVA(X)		CTagToHVA(X)
#define BindingOfCVA(X)		CTagToCVA(X)
#define BindingOfSVA(X)		CTagToSVA(X)
#define BindingOfStackvar(X)	X

#define BindSVA(U,V) \
{ \
  if (CondSVA(U)) \
    TrailPush(w->trail_top,U); \
  CTagToSVA(U) = V; \
}

#define BindCVA(U,V) \
{ \
  TrailPush(w->trail_top,U); \
  CTagToCVA(U) = V; \
}

#define BindHVA(U,V) \
{ \
  if (CondHVA(U)) \
    TrailPush(w->trail_top,U); \
  CTagToHVA(U) = V; \
}


#define Wake \
{ \
  SetEvent, Heap_Warn_Soft = HeapCharOffset(Heap_Warn_Soft,-1); \
}

#define WakeCount (TestEvent ? HeapCharDifference(Heap_Warn_Soft,Heap_Start) : 0)


#define PlainUntrail(TR,Ref,CONT) \
{ \
  Ref = TrailPop(TR); \
  if (!IsVar(Ref)) \
    CONT \
  else \
    CTagToPointer(Ref) = Ref; \
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
#define WAM_ABORT -32768 /* see intrinsics1.pl */

#include <setjmp.h>
#include "compat.h"

extern JMP_BUF abort_env;

extern void failc(char *mesg);

#define SERIOUS_FAULT(Y)       {failc(Y); \
                                LONGJMP(abort_env, WAM_ABORT); }
                          
#define MAJOR_FAULT(Y)         {failc(Y); return FALSE;}

#define USAGE_FAULT(Y)         {failc(Y); return FALSE;}

#define MINOR_FAULT(Y)         {return FALSE;}

/* Error codes, xref errors.pl //) */
#define INSTANTIATION_ERROR 1
#define READ_PAST_EOS_ERROR 2
#define NO_READ_PERMISSION 3
#define NO_WRITE_PERMISSION 4
#define NO_SUCH_FILE 5
#define NO_OPEN_PERMISSION 6
#define TYPE_ERROR(Type) (32+Type) /* includes also domain errors */

/* Type codes for TYPE_ERROR  //) */
#define ATOM 0
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

#define CHARACTER_CODE_LIST 32 /* First domain code */
#define STREAM_OR_ALIAS 33

#define BUILTIN_ERROR(Code,Culpr,ArgNo) \
   { ErrArgNo = ArgNo; Culprit = Culpr; return -Code; }

#define ERROR_IN_ARG(Arg,ArgNo,ReqType) \
{ ErrArgNo = ArgNo; Culprit = Arg; \
  return (IsVar(Arg) ? -INSTANTIATION_ERROR : -TYPE_ERROR(ReqType)); \
}


#define NDEREF(Wam, Reg,Aux,Exit) \
{ \
  DerefSwitch(Reg,Aux,{failc(illexp); Exit;}) \
  if (!IsNumber(Reg)) \
    { \
      Reg = evaluate(Wam, Reg); \
      if(!IsNumber(Reg)) \
        {failc(illexp); Exit;} \
    } \
}

/* MakeLST(To,Car,Cdr):
   
   Set 'To' to a term TAGGED LST
   whose car and cdr are 'Car' and Cdr'.

   'To' may be identical to 'Car' or 'Cdr'.
*/
#define MakeLST(To,Car,Cdr) \
{ TAGGED makelst_car = (Car); \
  HeapPush(w->global_top,makelst_car); \
  HeapPush(w->global_top,Cdr); \
  To = Tag(LST,HeapOffset(w->global_top,-2)); \
}

/* MakeSTR(To,Functor):
   
   Set 'To' to a term TAGGED STR
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
{ REGISTER TAGGED m_t0, m_u=U, m_t1=V; \
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

#define ENG_TTYPRINTF0(FMT) print_string(stream_user_error,FMT) 

#define ENG_TTYPRINTF1(FMT,A1) ENG_PRINTF1(stream_user_error,FMT,A1) 

#define ENG_TTYPRINTF2(FMT,A1,A2) ENG_PRINTF2(stream_user_error,FMT,A1,A2) 

#define ENG_TTYPRINTF3(FMT,A1,A2,A3) ENG_PRINTF3(stream_user_error,FMT,A1,A2,A3) 

#define ENG_TTYPRINTF4(FMT,A1,A2,A3,A4) ENG_PRINTF4(stream_user_error,FMT,A1,A2,A3,A4) 
