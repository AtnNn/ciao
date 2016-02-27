/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

/*         BASIC DATA DEFINITIONS  & SIMPLE MACROS
  the macros here involve casting,tagging detagging and the like
  term to pointer conversion must know where object are in virtual memory */


#ifndef _TERMDEFS_H
#define _TERMDEFS_H

#include "locks.h"
#include <setjmp.h> // JMP_BUF in misc_info_t
#include "compat.h" // JMP_BUF in misc_info_t
#include <stdio.h> // stream_node_t requires FILE

/* SIMPLE TYPES  & various CONSTANTS    -------------------------------   */

#define ANY 1                /* ?? or 0  - for dynamic arrays in bytecode */
#define SAME 0

typedef unsigned short int insn_t;                                /* 16 bit */
typedef unsigned long int tagged_t;                               /* 32 bit */
typedef unsigned long int UNtagged_t;                             /* 32 bit */

/* Pointer to bytecode */
/* todo: backport from optim-comp: the type of the pointer should
   really matter */
typedef insn_t *bcp_t;

typedef int bool_t;
#if !defined(_WINDOWS_H)
/* Win32 includes this definition */
# define FALSE 0
# define TRUE 1
#endif

typedef long ENG_INT;
typedef long long ENG_LINT;
typedef double ENG_FLT;

/* Typedefs for structs (C compiler needs them soon, since it only
   does one pass to resolve names in the source code).

   Ask me if you have any doubt (C is sometimes tricky) -- JFMC */

/* todo: re-organize .h as in optim_comp -- JFMC */

typedef struct worker_ worker_t;
typedef struct frame_ frame_t;
typedef struct node_ node_t;
typedef struct stream_node_ stream_node_t;
typedef struct goal_descriptor_ goal_descriptor_t; /* defined in task_areas.h */
typedef struct try_node_ try_node_t; /* defined in objareas.h */
typedef struct definition_ definition_t; /* defined in objareas.h */

/*** tagged_t DATATYPES --------------------------------***/

#if defined(DEBUG) || defined(PROFILE)
# define DEBUG_NODE /* Adds functor information in choicepoints
			to facilitate debugging */
#endif
/* # define DEBUG_NODE   */

#define WORDSIZE 32
#define SmallShift 2		/* no. of GC bits, concides with 32bit align */
#define TAGSIZE 3
#define ARITYSIZE 8
#define TAGOFFSET (WORDSIZE-TAGSIZE) /* 29 */
#define ARITYOFFSET (WORDSIZE-TAGSIZE-1-ARITYSIZE) /* 20 */
#define ARITYLIMIT (1<<ARITYSIZE) /* 256 */

#define TAGMASK		((((tagged_t)1<<TAGSIZE)-1)<<TAGOFFSET) /* E000 0000 */
#define QMask		((tagged_t)1<<TAGOFFSET>>1) /* 1000 0000 */
#define ZMask		((tagged_t)1<<TAGOFFSET>>2) /* 0800 0000 */
#define POINTERMASK	(QMask-(1<<SmallShift)) /* 0FFF FFFC */
#define INDEXMASK	(((tagged_t)1<<ARITYOFFSET)-1) /* 000F FFFF */
 
                     /* Tag creates tagged_t from tag and pointer */  
#if MallocBase
#define Tag(T,P)	(((T)<<TAGOFFSET)+((tagged_t)(P) & POINTERMASK))
#else
#define Tag(T,P)	(((T)<<TAGOFFSET)+((tagged_t)(P)))
#endif

#define TagIndex(T,P)	(((T)<<TAGOFFSET)+((tagged_t)((P)<<SmallShift)))

#define PointerPart(T)	((ENG_INT)((T)&POINTERMASK))  

#if MallocBase
#define TagToPointer(T)	((tagged_t *)(((tagged_t)(T)&POINTERMASK)+MallocBase))
#else
#define TagToPointer(T)	((tagged_t *)((tagged_t)(T)&POINTERMASK))
#endif

#define IndexPart(T)	(((T)&INDEXMASK)>>SmallShift)

#define HasTag(X,T)	(((X) & TAGMASK) == ((T)<<TAGOFFSET))
#define TagOf(P)	((P)>>TAGOFFSET)  /* collects tag */
#define CT(T1,T2)	((T1)<<TAGSIZE|(T2)) /* for concatenating tags     */

#define IsVar(A)	((int)(A)>=0)        /* variable tags begin with 0 */

#define TagIsHVA(X)	((X) < CVA<<TAGOFFSET)
#define TagIsCVA(X)	HasTag(X,CVA)
#define TagIsSVA(X)	((int)(X) >= (int)(SVA<<TAGOFFSET))
#define TagIsSmall(X)	((int)(X) < (int)TaggedHigh)
#define TagIsLarge(X)   (TagIsSTR(X) && STRIsLarge(X))
#define TagIsNUM(X)	((int)(X) < (int)(ATM<<TAGOFFSET)) 
#define TagIsATM(X)	HasTag(X,ATM)
#define TagIsLST(X)	HasTag(X,LST)
#define TagIsSTR(X)	((X) >= (STR<<TAGOFFSET))
#define TagIsStructure(X) (TagIsSTR(X) && !STRIsLarge(X))
#define STRIsLarge(X)   (TagToHeadfunctor(X) & QMask)

#if MallocBase
#define TermToPointer(X)	((tagged_t *)((X) ^ (TaggedZero^MallocBase)))
#define TermToPointerOrNull(X)	((tagged_t *)((X)==TaggedZero ? 0 : \
					    (X) ^ (TaggedZero^MallocBase)))
#define PointerToTerm(X)	((tagged_t)(X) ^ (TaggedZero^MallocBase))
#define PointerToTermOrZero(X)	(!(X) ? TaggedZero : \
				 (tagged_t)(X) ^ (TaggedZero^MallocBase))
#else
#define TermToPointer(X)	((tagged_t *)((X) ^ TaggedZero))
#define TermToPointerOrNull(X)	((tagged_t *)((X) ^ TaggedZero))
#define PointerToTerm(X)	((tagged_t)(X) ^ TaggedZero)
#define PointerToTermOrZero(X)	((tagged_t)(X) ^ TaggedZero)
#endif

/* Assuming IsVar(X): */
#define VarIsCVA(X)	((int)(X<<1) >= (int)(CVA<<1<<TAGOFFSET))

/* Assuming !IsVar(X): */
#define TermIsATM(X)	((int)(X<<1) >= (int)(ATM<<1<<TAGOFFSET))
#define TermIsLST(X)	((int)(X<<1) < (int)(STR<<1<<TAGOFFSET))

/* Test for HVA, CVA, LST, STR i.e. 0, 1, 6, 7 (and LNUM)*/
/* This works for some machines, but not for others...
   #define IsHeapTerm(A)	((int)(A)+(SVA<<TAGOFFSET)>=0)
*/
#define IsHeapTerm(A)	((tagged_t)(A)+(SVA<<TAGOFFSET) < (NUM<<TAGOFFSET))

#define IsHeapVar(X)	((X) < (SVA<<TAGOFFSET))
#define IsStackVar(X)	((int)(X) >= (int)(SVA<<TAGOFFSET))
#define IsAtomic(X)	((int)(X) < (int)(LST<<TAGOFFSET))
#define IsComplex(X)	((X) >= (LST<<TAGOFFSET))

#define TermIsAtomic(X) (IsAtomic(X) || TagIsLarge(X))
#define TermIsComplex(X) (IsComplex(X) && !TagIsLarge(X))

#define TagBitFunctor  (1<<TAGOFFSET)       /* ATM or STR or large NUM */
#define TagBitComplex  (2<<TAGOFFSET)       /* LST or STR or large NUM */

#define TagBitCVA (1<<TAGOFFSET) /* CVA (or UBV) */
#define TagBitSVA (2<<TAGOFFSET) /* SVA (or UBV) */

/* If this ordering ever changes, must update TagToHVA etc. too! */

#define HVA ((tagged_t)0)		/* heap variable */
#define CVA ((tagged_t)1)		/* constrained variable */
#define SVA ((tagged_t)2)		/* stack variable */
#define UBV ((tagged_t)3)		/* Unbound -- low bits are array index */

#define NUM ((tagged_t)4)		/* number: small integer */
#define ATM ((tagged_t)5)		/* atom: low part is atmtab index */
#define LST ((tagged_t)6)		/* list */
#define STR ((tagged_t)7)		/* structure */

#define MAXTAG 7
#define NOTAG 8

#define ERRORTAG   ((tagged_t)0)	/* ERRORTAG is a tagged_t pointer guaranteed 
				   to be different from all tagged_t objects */

/* Tags + one more bit: 
   Funny objects are represented as small ints.

   Floats and integers > 26 bits are represented as structures with
   special functors. The functors have the subtag bit = 1.
   Float=NUM, integer=ATM.

   ATM = atom as index in atmtab.
*/

#define TaggedLow	Tag(NUM,0)
#define TaggedZero	(TaggedLow+ZMask)
#define TaggedHigh	(TaggedLow+QMask)
#define HighInt		(1<<25)

/* These should be used with caution. */
#define MakeSmall(X)	(((tagged_t)((ENG_INT)(X)<<SmallShift))+TaggedZero)
#define GetSmall(X)	((ENG_INT)(((X)>>SmallShift)-(TaggedZero>>SmallShift)))
#define GetString(X)	(TagToAtom(X)->name)

#define USE_ATOM_LEN

#if defined(USE_ATOM_LEN)
#define GetAtomLen(X)   (TagToAtom(X)->atom_len)
#endif

#define MakeFunctorFix   (TagIndex(ATM,2) + QMask)
#define MakeFunctorFloat (TagIndex(NUM,3) + QMask)
#define LargeIsFloat(X)  (!(TagToHeadfunctor(X)&TagBitFunctor))

#define MakeLarge(ARG,Ptr) make_large(ARG,(tagged_t *)(Ptr))
#define MakeInteger(ARG, X) (IntIsSmall(X) ? MakeSmall(X) : make_integer(ARG,X))
#define MakeFloat(ARG,X)	make_float(ARG,X)
#define MakeAtom(X)	TagIndex(ATM,X)
#define MakeString(X)	init_atom_check(X)

#define GetInteger(X)	(TagIsSmall(X) ? GetSmall(X) : get_integer(X))
#define GetFloat(X)     (TagIsSmall(X) ? (ENG_FLT)GetSmall(X) : get_float(X))

#define IntIsSmall(X)	((X) >= -HighInt && (X) < HighInt)
#define IsInteger(X)	(TagIsSmall(X) || (TagIsLarge(X) && !LargeIsFloat(X)))
#define IsFloat(X)	(TagIsLarge(X) && LargeIsFloat(X))
#define IsNumber(X)	(TagIsSmall(X) || TagIsLarge(X))
#define IsAtom(X)	TagIsATM(X)
#define IsString(X)	TagIsATM(X)

/* tagged_t TO POINTER and related -----------------------------------*/
/* manipulating tagged_t objects removing tag and getting correct type of
   pointer  */

#if !defined(MallocBase)
# define CTagToHVA(x) (*(tagged_t *)(x))
# define CTagToCVA(x) (*(tagged_t *)(x))
# define CTagToSVA(x) (*(tagged_t *)(x))
# define CTagToLST(x) (*(tagged_t *)(x))
# define CTagToSTR(x) (*(tagged_t *)(x))
# define CTagToGoal(x) (*HeapOffset((tagged_t *)(x),1))
# define CTagToDef(x) (*HeapOffset((tagged_t *)(x),2))
# define CTagToCar(x) (*(tagged_t *)(x))
# define CTagToCdr(x) (*HeapOffset((tagged_t *)(x),1))
# define CTagToArg(x,I) (*HeapOffset((tagged_t *)(x),I))
# define CTagToPointer(x) (*TagToPointer(x)) /* must mask */
#else
# define CTagToHVA(x) (*TagToHVA(x))
# define CTagToCVA(x) (*TagToCVA(x))
# define CTagToSVA(x) (*TagToSVA(x))
# define CTagToLST(x) (*TagToLST(x))
# define CTagToSTR(x) (*TagToSTR(x))
# define CTagToGoal(x) (*TagToGoal(x))
# define CTagToDef(x) (*TagToDef(x))
# define CTagToCar(x) (*TagToCar(x))
# define CTagToCdr(x) (*TagToCdr(x))
# define CTagToArg(x,I) (*TagToArg(x,I))
# define CTagToPointer(x) (*TagToPointer(x))
#endif


#if sun && mc68010
/* This gets around a compiler error! */
# define TagToHVA(x)	((tagged_t *)(x)) /* tag = 0 */
# define TagToCVA(x)	TagToPointer(x)
# define TagToSVA(x)	TagToPointer(x)
# define TagToUBV(x)	TagToPointer(x)
# define TagToLST(x)	TagToPointer(x)
# define TagToSTR(x)	TagToPointer(x)

# define TagHVA(x)	((tagged_t)(x)) /* tag = 0 */
# define TagSVA(x)	Tag(SVA,x)
# define TagCVA(x)	Tag(CVA,x)
#else
#if MallocBase
# define TagToHVA(x)	((tagged_t*)((x)+MallocBase)) /* tag = 0 */
# define TagToCVA(x)	((tagged_t*)((x)-(CVA<<TAGOFFSET)+MallocBase))
# define TagToSVA(x)	((tagged_t*)((x)-(SVA<<TAGOFFSET)+MallocBase))
# define TagToUBV(x)	((tagged_t*)((x)-(UBV<<TAGOFFSET)+MallocBase))
# define TagToLST(x)	((tagged_t*)((x)-(LST<<TAGOFFSET)+MallocBase))
# define TagToSTR(x)	((tagged_t*)((x)-(STR<<TAGOFFSET)+MallocBase))

# define TagHVA(x)	Tag(HVA,x)
# define TagSVA(x)	Tag(SVA,x)
# define TagCVA(x)	Tag(CVA,x)
#else
# define TagToHVA(x)	((tagged_t*)(x)) /* tag = 0 */
# define TagToCVA(x)	((tagged_t*)((x)-(CVA<<TAGOFFSET)))
# define TagToSVA(x)	((tagged_t*)((x)-(SVA<<TAGOFFSET)))
# define TagToUBV(x)	((tagged_t*)((x)-(UBV<<TAGOFFSET)))
# define TagToLST(x)	((tagged_t*)((x)-(LST<<TAGOFFSET)))
# define TagToSTR(x)	((tagged_t*)((x)-(STR<<TAGOFFSET)))

# define TagHVA(x)	((tagged_t)(x)) /* tag = 0 */
# define TagSVA(x)	Tag(SVA,x)
# define TagCVA(x)	Tag(CVA,x)
#endif
#endif


/* Functor hackery. --MC */
/*-----------------------*/

/* 1 + no. untyped words */
#define LargeArity(X)	(PointerPart(X)>>2)

#define LargeInsns(X)	(PointerPart(X)>>1)

#define Arity(X)	(PointerPart(X)>>ARITYOFFSET)
#define SetArity(X,A)	((tagged_t)(((X) & (TAGMASK | INDEXMASK)) | (A<<ARITYOFFSET)))

#define TagToAtom(X)	(atmtab[IndexPart(X)]->value.atomp)

    /* finding the principal functor of a structure */
    /* finding the arguments of a structure, first argument is 1 */
    /* finding the car & cdr of a list. */
    /* finding the constraints of a CVA. */
#define TagToHeadfunctor(X) CTagToSTR(X)
#define TagToArg(X,N)	HeapOffset(TagToSTR(X),N)
#define TagToCar(X)	TagToLST(X)
#define TagToCdr(X)	HeapOffset(TagToLST(X),1)
#define TagToGoal(X)	HeapOffset(TagToCVA(X),1)
#define TagToDef(X)	HeapOffset(TagToCVA(X),2)

#define TagToInstance(X)	((instance_t *)TermToPointerOrNull(X))
#define TagToInstHandle(X)	((instance_handle_t *) TermToPointerOrNull(X))
#define TagToInstancePtr(X)	((instance_t **)TermToPointerOrNull(X))
#define TagToStream(X)	((stream_node_t *)TermToPointer(X))
#define TagToLock(X)	((LOCK *)TermToPointer(X))
#define TagToSLock(X)	((SLOCK *)TermToPointer(X))
#define TagToBool(X)	((bool_t *)TermToPointer(X))
#define TagToRoot(X)	((int_info_t *)TermToPointer(X))
#define TagToEmul(X)	((emul_info_t *)TermToPointer(X))
#define TagToFunctor(X)	((definition_t *)TermToPointer(X))

#if defined(ANDPARALLEL)

typedef struct parallel_exec_entry_ parallel_exec_entry_t;
typedef struct handler_entry_ handler_entry_t;
typedef struct event_entry_ event_entry_t;
typedef struct par_handler_ par_handler_t;

/* HANDLER structure */

struct par_handler_ {
  tagged_t goal;                              /* Parallel goal */
  bool_t det;                                 /* Deterministic goal? */
  ENG_INT exec_state;                       /* Parallel execution state */
  worker_t *agent;                     /* Pointer to publishing agent */
  worker_t *remote_agent;              /* Pointer to stealing agent */
  parallel_exec_entry_t *exec_limits;  /* Limits of parallel execution */
  handler_entry_t *gle;                /* Entry into the goal list */
#if defined (VISANDOR)
  ENG_INT ppf;                              /* Id */
#endif
};

/* PARALLEL EXECUTIONS LIST */
struct parallel_exec_entry_ {
  node_t *init;                 /* Pointer to beginning of goal exec */
  node_t *end;                  /* Pointer to end of goal exec */
  node_t *wipe;                 /* Pointer to previous not dead exec */
  tagged_t *trail_top;                 /* Pointer to save current trail top */
  parallel_exec_entry_t *prev;  /* Pointer to previous one */
  parallel_exec_entry_t *next;  /* Pointer to next one */
};

/* GOAL LIST */
struct handler_entry_ {
  tagged_t handler;              /* Handler */
  handler_entry_t *prev;  /* Pointer to previous one */
  handler_entry_t *next;  /* Pointer to next one */
};

/* EVENT QUEUE */
struct event_entry_ {
  tagged_t handler;                      /* Handler */
  bool_t canc;                           /* Cancellation or backtracking? */
  parallel_exec_entry_t *limits;  /* Execution limits */
  event_entry_t *prev;            /* Pointer to previous one */
  event_entry_t *next;            /* Pointer to next one */
};

/* Statistics */
typedef struct int_par_ int_par_t;
struct int_par_ {
  int value;
  int_par_t *prev;
  int_par_t *next;
};
typedef struct double_par_ double_par_t;
struct double_par_ {
  double time;
  double n;
  double_par_t *prev;
  double_par_t *next;
};

#if defined (VISANDOR)
/* EVENT for VisAndOr */
typedef struct visandor_event_ visandor_event_t;
struct visandor_event_ {
  unsigned long timestamp;
  int evtype;
  ENG_INT ppf;
  int nfork;
  worker_t *wam;
};
#endif
#endif

#if defined(PARBACK)

typedef struct goal_entry goal_entry_t;
typedef struct ans_list AL_t;
typedef struct parcall_frame PF_t;
typedef struct par_goal par_goal_t;

/* GOAL LIST */

struct goal_entry {
  par_goal_t *par_goal;              /* Handler */
  goal_entry_t *prev;  /* Pointer to previous one */
  goal_entry_t *next;  /* Pointer to next one */
};

struct ans_list {
  unsigned int numVars;
  tagged_t *vars;
  unsigned int start;
  unsigned int end;
  tagged_t *trail;
  tagged_t *pHeap;
  AL_t *next;
};

struct parcall_frame {
  PF_t *prev;                   /* Previous parallel call frame */
  SLOCK vars_l;                 /* SLOCK for combining */
  bool_t combining;             /* Some goal is combining */
  bool_t inside;                /* Some goal is combining */
//  bool_t backtracking;        /* is backtracking active? */
  unsigned int ansPending;      /* Number of goals which does not find any answer yet. */
  unsigned int failPending;     /* Number of goals which does not fail yet. */
  unsigned int numGoals;        /* Number of goals in the parcall. */
  par_goal_t *goals;            /* Goals of the parcall. */
};

/* PARALLEL GOAL structure */
struct par_goal {
  tagged_t *heapStart;
  tagged_t *localStart;
  tagged_t *trailStart;
  tagged_t goal;                /* Parallel goal */
  PF_t *pf;                     /* Parallel call frame */
  unsigned int mem_size;        /* Answers memory size */
  tagged_t *memory;             /* Memory to store answers */
  tagged_t *value_trail;        /* Value trail at time of finding the last answer (or suspension) */
  unsigned int free;            /* First free memory */
  tagged_t *array;              /* Memory to terms */
  AL_t *firstAns;               /* First produced answer */
  AL_t *nowComb;                /* Actual joined answer */
  AL_t *lastComb;               /* Last joined answer */
  AL_t *lastAns;                /* Last produced answer */
  node_t *ch_init;              /* Initial choice point */
  node_t *ch_end;               /* Final choice point */
  tagged_t *trail_init;         /* Initial trail pointer */
  tagged_t *trail_end;          /* Final trail pointer */
  bool_t combining;             /* is it combining its last answer? */
  ENG_INT exec_state;           /* Parallel execution state */
  worker_t *agent;              /* Pointer to publishing agent */
  worker_t *remote_agent;       /* Pointer to stealing agent */
  goal_entry_t *gle;            /* Entry into the goal-cancel-back list */
  ENG_INT list;                 /* Which list the handler belongs to? */
};

#endif

typedef struct numstack_ numstack_t;
struct numstack_ {
  tagged_t *end;
  numstack_t *next;
};

/* Other data structures */

/* Streams: a doubly linked circular list */
struct stream_node_ {
  tagged_t label;
  stream_node_t *backward;
  stream_node_t *forward;
  tagged_t streamname;
  char streammode;
  int pending_char;                                      /* From peek'ing */
  unsigned int isatty:1;
  unsigned int socket_eof:1;
  /*  unsigned int socket_is_unbuffered:1; -- Not used (DCG) */
  int previous_char; /* To correctly count lines in Mac, Win, Unix (DCG) */
  ENG_INT last_nl_pos;
  ENG_INT nl_count;
  ENG_INT char_count;
  FILE *streamfile;                               /* Not used for sockets */
};

/* WAM CIAO_REGISTERS */

/* There are some registers which whould be private to each worker.  Those
   which are not critical for speed are subindirected in their own blocks.
   Others I think can affect performance appear just at the worker level.
   The reason for this difference is that changing the size of the struct
   worker causes a real mess in the whole compiler, and a bootstrapping
   procedure is needed.  Thus, if new no critical registers are needed, they
   can be allocated inside of one of these blocks, and this does not change
   the size of the worker_t itself. */

typedef struct io_streams_ io_streams_t;
struct io_streams_ {
  stream_node_t *input_stream_ptr;
  stream_node_t *output_stream_ptr;
  stream_node_t *error_stream_ptr;
};

typedef struct debugger_state_ debugger_state_t;
struct debugger_state_ {
  tagged_t current_debugger_state;
  tagged_t current_debugger_mode;
};

#if defined(GLOBVARS)
#define MAXGLOBALVARS 16
#endif

#define USE_GLOBAL_VARS 1
#if defined(USE_GLOBAL_VARS)
#define GLOBAL_VARS_ROOT (w->misc->global_vars_root)
#endif

typedef struct misc_info_ misc_info_t;
struct misc_info_ {

/* Incore compilation of a clause sometimes requires expanding the number of
   X registers of the machine, which amounts to expanding the worker itself.
   In that case, new_worker (which is otherwise NULL) will point to the
   newly allocated worker.  This worker will be the active one upon normal
   returning to wam(). */

  worker_t *expanded_worker;

  /* From heapgc: need to have per-worker garbage collection */

  long gc_total_grey/* = 0 */; /* accumulated upper bound of garbage left */
                                         /* Must be explicitly inited !!! */
  long gcgrey;          /* upper bound(?) of garbage left in old segments */
  long total_found;        /* the number of non-garbage words in the heap */
  tagged_t cvas_found;                 /* the last CVA found while shunting */
  node_t *gc_aux_node;     /* aux. choicepoint for the WAM registers */
  node_t *gc_choice_start;
  tagged_t *gc_trail_start;
  tagged_t *gc_heap_start;
  frame_t *gc_stack_start;
  node_t *top_conc_chpt;  /* Topmost chicepoint for concurrent facts */
#if defined(GLOBVARS)
  tagged_t globalvar[MAXGLOBALVARS];                     /* Global registers */
#endif
#if defined(USE_GLOBAL_VARS)
  tagged_t global_vars_root;
#endif
  
   /* For error handling through exceptions */
  int errargno;
#if defined(USE_OC_EXCEPTIONS)
  int errcode;
  char *errfuncname;
  int errfuncarity;
#endif
  tagged_t culprit;

#if defined(USE_OC_EXCEPTIONS)
  JMP_BUF *errhandler;
#endif

  /* Access the goal descriptor for this thread */
  goal_descriptor_t *goal_desc_ptr;

  /* Available workers are enqueued in a list. */
  worker_t *next_worker;

  /* This goal should stop right now! */
  bool_t stop_this_goal;

#if defined(ANDPARALLEL)
  /* TO_DO: not very efficient solution. THEY SHOULD BE ADDED IN
     (WORKER_T) DIRECTLY, but it cannot be done directly.
     SOLUTION: generate another compiler with the suitable shifts,
     then a new engine, and finally put them together. */

  /* Goal list, pointers and lock */
  handler_entry_t *goal_list_start;  /* Start of goal list */
  handler_entry_t *goal_list_top;    /* Top of goal list */
  SLOCK goal_list_l;

  /* Event Queue, pointers and lock */
  event_entry_t *event_queue_start;  /* Start of event queue */
  event_entry_t *event_queue_top;    /* Top of event queue */
  SLOCK event_queue_l;

  /* Pointers to limits of goal executions */
  parallel_exec_entry_t *last_parallel_exec;
  SLOCK parallel_exec_l;
  node_t *node_top;
  tagged_t *trail_top_global;

  /* Lock for creating mutual exclusions */
  SLOCK mutex_l;

  /* For expanding stacks */
  volatile int suspend;     /* Suspend the execution? */

  /* For cancellation */
  volatile par_handler_t *cancel_goal_exec;  /* Cancel goal execution */
  volatile bool_t safe_to_cancel;    /* Can the thread safely cancel it? */
  node_t *current_init_chp;   /* Initial ch.p. of current execution */
  tagged_t *current_trail_top;       /* Trail pointer of current execution */

  /* Suspend until stolen goal finished or goals available */
  LOCK waiting_for_work_l;       /* Waiting for more work to do (lock) */
  COND_VAR waiting_for_work_cv;  /* Waiting for more work to do (cond_var) */
  int mode;                      /* Executing backwards or forwards */
  volatile bool_t suspended_waiting_for_work;  /* Flag */

#if defined(Solaris)
  /* Measure of total suspending time of the agent */
  double_par_t *total_suspending_time;        /* Measure */
  hrtime_t suspending_time_cont;                     /* Measure */
#endif

  /* Pointer to next WAM */
  worker_t *next_wam;

#if defined(VISANDOR)
  unsigned long my_count_calls;            /* compensate clock time */
  float my_time_each_call;                 /* helps the former */
  ENG_INT pcall_level;
  visandor_event_t *tmpevptr;
  visandor_event_t *nextevent;  /* current pointer into event array for this agent */
  visandor_event_t *firstevent;
  visandor_event_t *lastevent;
#endif
#endif

#if defined(PARBACK)
  /* Goal list, pointers and lock */
  goal_entry_t *goal_list_start;  /* Start of goal list */
  goal_entry_t *goal_list_top;    /* Top of goal list */
  goal_entry_t *cancel_goal_list_start;  /* Start of goal list */
  goal_entry_t *cancel_goal_list_top;    /* Top of goal list */
  goal_entry_t *back_goal_list_start;  /* Start of goal list */
  goal_entry_t *back_goal_list_top;    /* Top of goal list */
  frame_t *contFrame;

  PF_t *pf;
  
  /* Lock for creating mutual exclusions */
  SLOCK mutex_l;

  int suspend;     /* Suspend the execution? */

  /* For cancellation */
  par_goal_t *goal_to_cancel;  /* Goal to be cancelled */
  bool_t safe_to_cancel;    /* Can the thread safely cancel it? */

  /* Suspend until stolen goal finished or goals available */
  LOCK waiting_for_work_l;       /* Waiting for more work to do (lock) */
  COND_VAR waiting_for_work_cv;  /* Waiting for more work to do (cond_var) */
  bool_t suspended_waiting_for_work;  /* Flag */

  worker_t *next_wam;
#endif
};

struct worker_ {
  /* Space for miscelaneous stuff (or temporary hacks) */
  misc_info_t *misc;

  /* Input and output streams */
  io_streams_t *streams;

  /* Current debugger state.  Realloced into local heap. */
  debugger_state_t *debugger_info;

  /* Save info for incore compilation */
  bcp_t last_insn;

  /* Temporary allocation for various operations regarding atoms and strings. */
  char *atom_buffer;
  int atom_buffer_length;

  /* Private stack for operations with big numbers.  Better have fast maths. */
  tagged_t *numstack_top;
  tagged_t *numstack_end;
  numstack_t *numstack_first;
  numstack_t *numstack_last;

  /* Boundaries of different areas */
  tagged_t *heap_start;
  tagged_t *heap_end;
  tagged_t *heap_warn_soft;
  tagged_t *heap_warn;
  tagged_t *int_heap_warn;      /* Heap_Start if ^C was hit, else Heap_Warn */
    
  tagged_t *stack_start;
  tagged_t *stack_end;
  tagged_t *stack_warn;
    
  tagged_t *choice_end;
  tagged_t *choice_start;


#if defined(USE_TAGGED_CHOICE_START)
  tagged_t *tagged_choice_start;   /* Not used any more, but I can't just
                                    remove it: the size of the WRB is
                                    critical for the compiler and changing
                                    it is a real hassle */
#else
  tagged_t *dummy;                           /* Use up the space, therefore */
#endif    

  tagged_t *trail_start;
  tagged_t *trail_end;

  node_t *node;		/* choice pointer */
  node_t *next_node;	/* -""- at predicate entry */
  node_t *segment_node;	/* gc's segment choice point */
  bcp_t insn;			/* program counter */
  tagged_t *structure;		/* structure pointer */
  tagged_t global_uncond;		/* first uncond. global variable */
  tagged_t local_uncond;		/* first uncond. local variable no. */
  int value_trail;		/* size of value_trail extension of w->node */

  /* incidentally, the rest is similar to a node_t */
  tagged_t *trail_top;		/* trail pointer */
  tagged_t *global_top;		/* heap pointer */
  try_node_t *next_alt;	/* next clause at predicate entry */
  frame_t *frame;		/* environment pointer */
  bcp_t next_insn;		/* continuation */
  frame_t *local_top;	/* local stack pointer, or NULL if invalid */
  tagged_t term[ANY];		/* temporary variables */
};


struct frame_ {			/* a.k.a. environment */
  frame_t *frame;		/* continuation frame pointer */
  bcp_t next_insn;		/* continuation program pointer */
  tagged_t term[ANY];		/* permanent variables */
};

typedef enum {CHOICE,MARKER} node_type;

struct node_ {			/* a.k.a. marker. Collapsed with a Chpt? */
/* #if defined(MARKERS) */
  /*  node_type type_of_node;*/
/* #endif */
  tagged_t *trail_top;
  tagged_t *global_top;
  try_node_t *next_alt;
  frame_t *frame;
  bcp_t next_insn;
  frame_t *local_top;
#if defined(DEBUG_NODE)
  definition_t *functor;
#endif

  tagged_t term[ANY];            /* Offset between nodes in try_node struct */
};

#if defined(MARKERS)

typedef struct extension_regs_ extension_regs_t;
struct extension_regs_ {
  marker_t *topmost_marker;                    /* Last pushed marker */
};

typedef struct marker_ marker_t;
struct marker_ {
  node_type type_of_node;
  marker_t* previous_marker;
  marker_t* next_marker;                         /* NULL if topmost */
  worker_t saved_state;              /* Huge, but should be reduced */
};

#endif

typedef unsigned long Bignum;

#define ChoiceptMarkPure(b) (*(tagged_t *)(&(b)->trail_top) |= 1)
#define ChoiceptTestPure(b) ((tagged_t)(b)->trail_top & 1)

#define ChoiceptMarkStatic(b) (*(tagged_t *)(&(b)->trail_top) |= 2)
#define ChoiceptTestStatic(b) ((tagged_t)(b)->trail_top & 2)

#if (MallocBase>>28 & 0x8)
#define ChoiceptMarkNoCVA(b) (*(tagged_t *)(&(b)->trail_top) &= ~0x80000000)
#define ChoiceptTestNoCVA(b) (!((tagged_t)(b)->trail_top & 0x80000000))
#else
#define ChoiceptMarkNoCVA(b) (*(tagged_t *)(&(b)->trail_top) |= 0x80000000)
#define ChoiceptTestNoCVA(b) ((tagged_t)(b)->trail_top & 0x80000000)
#endif

/*  Misc   ---------------------------------------------  */

#define BPL		(sizeof(long)/sizeof(insn_t))
#define BPT		(sizeof(tagged_t)/sizeof(insn_t))
#define BPTP		(sizeof(tagged_t *)/sizeof(insn_t))
#define BPF		(sizeof(ENG_FLT)/sizeof(insn_t))

/* Deposit Source into Mask:ed portion of Dest */
#define Deposit(Source,Mask,Dest) (((Source)&(Mask))|((Dest)& ~(Mask)))

#endif /* _TERMDEFS_H */
