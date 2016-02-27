/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include "datadefs.h"
#include "support.h"
#include "support_defs.h"
#include "term_support_defs.h"
#include "threads.h"
#include "locks.h"
#include "initial.h"
#include "task_areas.h"
#include "wam.h"
#include "wam_defs.h"
#include "tasks_defs.h"
#include "startgoal_defs.h"
#include "nondet_defs.h"
#include "start_defs.h"
#include "alloc_defs.h"
#include "ciao_prolog.h"
#include "streams_defs.h"
#include "predtyp.h"

#include "math.h"
#include <sys/time.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(PARBACK)

/* -------------- */
/*      Tags      */
/* -------------- */

#define VarTrie      0xE0000001  //ID of trie vars
#define TrieVarIndex(TERM)  (((TERM) & 0x0FFFFFFF) >> 1)

#define EMPTY_LIST 0xA00000F0
#define INTEGER_MARK 0xB0000008
#define FLOAT_MARK 0x9000000C

///// MEMORY SIZE ////
#define MEMSIZE  10000
#define TERMSIZE 100

#define MkVarTerm(Arg) (my_make_var(Arg))
#define MkPairTerm(HEAD,TAIL) (my_make_list(Arg,(HEAD),(TAIL)))

/* ------------------------ */
/*      Destruct Terms      */
/* ------------------------ */
#define HeadOfList(TERM) (CTagToPointer(TERM))
#define TailOfList(TERM) (*(TagToPointer(TERM) + 1))
#define ArityOfFunctor(FUNCTOR)(Arity(TagToHeadfunctor(FUNCTOR)))
#define Alig(PTR) ((tagged_t*)(((tagged_t)PTR) & 0xFFFFFFFD))
#define Change(T1,T2) (T1 = ((tagged_t*)((((tagged_t)T1) & 0x00000003) | (tagged_t)Alig(T2))))

/* -------------------- */
/*      Test Terms      */
/* -------------------- */
#define IsFreeVar(X) (IsVar(X) && ((X) == CTagToPointer(X)))
#define IsTrieVar(TERM)  (((TERM) & 0xF0000001) == VarTrie)

/* -------------------- */
/*      Unification     */
/* -------------------- */
#define Unify(TERM1,TERM2) (cunify(Arg,(TERM1),(TERM2)))

#if defined(THREADS) && defined(USE_POSIX_THREADS)
extern pthread_attr_t joinable_thread;
#endif

try_node_t *address_nd_fork_c;
try_node_t *address_nd_join_c;

tagged_t fifo;
tagged_t lifo;
tagged_t det;

bool_t show_cancellation;
bool_t show_move_top;
bool_t show_fork;
bool_t show_join;
bool_t show_new_answer;
bool_t show_state;
bool_t show_limits;
bool_t show_lists;
bool_t show_mutex;
bool_t show_all;

/* state of execution */
#define NOT_STARTED    0
#define REM_EXECUTING  1
#define ANSWER_FOUND   2
#define FAILED         3
#define CANCELLED      4
#define SPEC_SUSPENDED 5

#define NUMBER_STATES  6

#define GOAL    0
#define CANCEL  1
#define BACK    2
#define NONE    3

/* Checks whether a particular variable is a callable structure or not */
#define NOT_CALLABLE(What) (IsVar(What) || TagIsSmall(What) || TagIsLarge(What))
#define ENSURE_CALLABLE(What, ArgNum)					\
  if (NOT_CALLABLE(What))  { fprintf(stderr, "\nparback module: NOT CALLABLE GOAL"); }

#define SaveValueTrailAndUntrailStateGoal(GOAL)				\
  {									\
    if (show_join)							\
      {									\
	printf("JOIN %p: Saving from %p until %p\n",Arg,Alig(GOAL.trail_init),Alig(GOAL.trail_end)); \
	fflush(stdout);							\
      }									\
    if (show_join)							\
      {									\
	printf("JOIN %p: %d trail cells\n",Arg,TrailDifference(Alig(GOAL.trail_init),Alig(GOAL.trail_end))); \
	fflush(stdout);							\
      }									\
    if (TrailCharDifference(Alig(GOAL.trail_init),Alig(GOAL.trail_end)) > 0) \
      {									\
	GOAL.value_trail =						\
	  checkalloc(TrailCharDifference(Alig(GOAL.trail_init),Alig(GOAL.trail_end))); \
	tagged_t *value_trail;						\
	value_trail = GOAL.value_trail;					\
	tagged_t *ind;							\
	for (ind = Alig(GOAL.trail_init); TrailYounger(Alig(GOAL.trail_end),ind); ind++) \
	  {								\
	    /* Taking the actual binding */				\
	    *value_trail = CTagToPointer(CTagToPointer(ind));		\
	    if (show_join) { printf("JOIN %p: trail %p = %p -> %p\n",Arg, ind, CTagToPointer(ind),*value_trail); fflush(stdout); } \
	    /* The variable is unbound */				\
	    CTagToPointer(CTagToPointer(ind)) = CTagToPointer(ind);	\
	    printf("Borro %p\n",ind);					\
	    value_trail++;						\
	  }								\
      }									\
  }

#define SaveValueTrailAndUntrailState(PF)				\
  {									\
    int i;								\
    for (i = 0; i < PF->numGoals; i++)					\
      {									\
	if ((PF->goals[i].exec_state == ANSWER_FOUND) &&		\
	    (PF->goals[i].ch_init != PF->goals[i].ch_end))			\
	  {								\
	    if (show_join) { printf("JOIN %p: Saving goal %d\n",Arg,i); fflush(stdout); } \
	    SaveValueTrailAndUntrailStateGoal(PF->goals[i]);		\
	  }								\
      }									\
  }

#define FreeValueTrailAndRedoStateGoal(GOAL)				\
  {									\
    tagged_t *ind;							\
    tagged_t *value_trail;						\
    value_trail = GOAL.value_trail + TrailDifference(Alig(GOAL.trail_init),Alig(GOAL.trail_end)) - 1; \
    /* Redo the previous binding */					\
    if (show_join) { printf("JOIN %p: Redoing from %p until %p\n",Arg,Alig(GOAL.trail_init),Alig(GOAL.trail_end)); fflush(stdout); } \
    for (ind = Alig(GOAL.trail_end) - 1; !TrailYounger(Alig(GOAL.trail_init), ind); ind--) \
      {									\
	if (show_join) { printf("JOIN %p: trail %p = %p\n",Arg, CTagToPointer(ind),*value_trail); fflush(stdout); } \
	CTagToPointer(CTagToPointer(ind)) = *value_trail;		\
	printf("Rehago %p\n",ind);					\
	value_trail--;							\
      }									\
    if (TrailCharDifference(Alig(GOAL.trail_init),Alig(GOAL.trail_end)) > 0) \
      {									\
	checkdealloc(GOAL.value_trail,					\
		     TrailCharDifference(Alig(GOAL.trail_init),Alig(GOAL.trail_end))); \
      }									\
  }

#define FreeValueTrailAndRedoState(PF)					\
  {									\
    int i;								\
    for (i = (PF)->numGoals - 1; i >= 0; i--)				\
      {									\
	if ((PF->goals[i].exec_state == ANSWER_FOUND) &&		\
	    (PF->goals[i].ch_init != PF->goals[i].ch_end))			\
	  {								\
	    if (show_join) { printf("JOIN %p: Redoing goal %d\n",Arg,i); fflush(stdout); } \
	    FreeValueTrailAndRedoStateGoal((PF)->goals[i]);		\
	  }								\
      }									\
  }

#define InitializeNowComb(PF)						\
  {									\
    int i;								\
    for (i = 0; i < (PF)->numGoals; i++)				\
      {									\
	if ((PF)->goals[i].combining && (comb == -1))			\
	  {								\
	    (PF)->goals[i].nowComb = (PF)->goals[i].lastComb;		\
	    comb = i;							\
	  }								\
	else								\
	  {								\
	    (PF)->goals[i].combining = FALSE;				\
	    (PF)->goals[i].nowComb = (PF)->goals[i].firstAns;		\
	  }								\
	if (show_join) { printf("JOIN %p: Comb of %d = %p\n",Arg,i,(PF)->goals[i].nowComb); fflush(stdout); } \
      }									\
    Wait_Acquire_slock(PF->vars_l);					\
    Release_slock(PF->vars_l);						\
  }								

#define VarBelongsToMemoryGoal(IVAR,GOAL)				\
  ((IsVar(IVAR) && (TagToPointer(*(IVAR)) > (GOAL).memory)) &&		\
   (TagToPointer(*(IVAR)) < ((GOAL).memory + (GOAL).free)))			

#define MoveToHeap(IVAR,GOAL,ANS)					\
  (*(IVAR) - (tagged_t)((GOAL).memory + (ANS)->start) + (tagged_t)(ANS)->pHeap) 


#define TrailUnifyAns(ANS)						\
  {									\
    int j;								\
    for (j = 0; j < (ANS)->numVars; j++)				\
      {									\
	Unify((ANS)->vars[j], (ANS)->pHeap[j]);				\
      }									\
  }

#define NowCombAnswerToHeap(GOAL)					\
  {									\
    tagged_t *ind;							\
    if ((GOAL).nowComb->numVars > 0)					\
      {									\
	ciao_ensure_heap(Arg->misc->goal_desc_ptr,			\
			 (GOAL).nowComb->end - (GOAL).nowComb->start);	\
	(GOAL).nowComb->pHeap = Arg->global_top;			\
	for (ind = (GOAL).memory + (GOAL).nowComb->start;		\
	     ind < (GOAL).memory + (GOAL).nowComb->end; ind++)		\
	  {								\
	    if (VarBelongsToMemoryGoal(ind,(GOAL)) ||			\
		TagIsStructure(*ind) || TagIsLST(*ind))			\
	      {								\
		HeapPush(Arg->global_top,MoveToHeap(ind,(GOAL),(GOAL).nowComb)); \
	      }								\
	    else							\
	      {								\
		HeapPush(Arg->global_top,*ind);				\
	      }								\
	    Arg->global_top--;						\
	    if (show_join) { printf("JOIN %p: Apilo %p\n",Arg,CTagToPointer(Arg->global_top)); fflush(stdout); } \
	    Arg->global_top++;						\
	  }								\
      }									\
    									\
    /* Unification of the variables without going to the trail */	\
    /* We will do untrailing by hand in nd_join_c */			\
    (GOAL).nowComb->trail = Arg->trail_top;				\
    if (show_join) { printf("JOIN %p: PRE Unifying trail %p\n",Arg,Arg->trail_top); fflush(stdout); } \
    TrailUnifyAns((GOAL).nowComb);					\
    if (show_join) { printf("JOIN %p: POST Unifying trail %p\n",Arg,Arg->trail_top); fflush(stdout); } \
  }

#define NowCombAnswerForEachGoalToHeap(PF)				\
  {									\
    if (show_join) { printf("JOIN %p: Reusing %d numVars %d\n",Arg,comb,(PF)->goals[comb].nowComb->numVars); fflush(stdout); } \
    NowCombAnswerToHeap((PF)->goals[comb]);				\
    int i;								\
    for (i = 0 ; i < (PF)->numGoals; i++)				\
      {									\
	if (i == comb) continue;					\
	if (show_join) { printf("JOIN %p: Reusing %d numVars %d\n",Arg,i,(PF)->goals[i].nowComb->numVars); fflush(stdout); } \
	NowCombAnswerToHeap((PF)->goals[i]);				\
      }									\
  }

#define NoMoreCombinig(PF)					\
  {								\
    int iGoals;							\
    for (iGoals = 0; iGoals < (PF)->numGoals; iGoals++)		\
      {								\
	if ((PF)->goals[iGoals].combining)			\
	  {							\
	    (PF)->goals[iGoals].combining = FALSE;		\
	    break;						\
	  }							\
      }								\
  }

#define PutAnswerBindingsFromHeap(ANS)					\
  {									\
    int j;								\
    if (show_join) { printf("JOIN %p: PRE Unifying trail %p\n",Arg,Arg->trail_top); fflush(stdout); } \
    ANS->trail = Arg->trail_top;					\
    for (j = 0; j < (ANS)->numVars; j++)				\
      {									\
	Unify((ANS)->vars[j], (ANS)->pHeap[j]);				\
      }									\
    if (show_join) { printf("JOIN %p: POST Unifying trail %p\n",Arg,Arg->trail_top); fflush(stdout); } \
  }

#define PutPHeapToNULL(PF)				\
  {							\
    int i;						\
    for (i = 0; i < (PF)->numGoals; i++)		\
      {							\
	AL_t *auxAns = (PF)->goals[i].firstAns;		\
	while (auxAns != NULL)				\
	  {						\
	    auxAns->pHeap = NULL;			\
	    auxAns = auxAns->next;			\
	  }						\
      }							\
  }

#endif
