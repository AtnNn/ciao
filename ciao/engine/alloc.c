/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP */


#if defined(Solaris) || defined(LINUX) || defined(DARWIN) || defined(BSD)
#include <string.h>
#else
#include <memory.h>
#endif


#include <unistd.h>

#include "compat.h"
#include "datadefs.h"
#include "support.h"

/* declarations for global functions accessed here */

#include "own_malloc_defs.h"
#include "start_defs.h"
#include "alloc_defs.h"
#include "support_defs.h"
// #include "streams_defs.h"
#include "wamsupport_defs.h"
#include "timing_defs.h"
#include "initial_defs.h"
#include "registers.h"

/* local declarations */
char * tryalloc_errstring;

#define MEMORY_FAULT(message) {			\
    tryalloc_errstring = message;		\
    return NULL;				\
  }

#define CHECK_FOR_MEMORY_FAULT(alloc_call)  {	\
    tagged_t * p = alloc_call;			\
    if (!p) SERIOUS_FAULT(tryalloc_errstring);	\
    return p;					\
  }

/* segfault patch -- jf */
#if defined(MallocBase)     
#define ENSURE_ADDRESSABLE(P, SIZE) \
  (((tagged_t)(P) >= (tagged_t)MallocBase) && \
  (((tagged_t)(P) - (tagged_t)MallocBase) < POINTERMASK) && \
   (((tagged_t)(P) - (tagged_t)MallocBase + (SIZE)) < POINTERMASK))
#else
#define ENSURE_ADDRESSABLE(P, SIZE) \
  ((tagged_t)(P) < POINTERMASK)
#endif

static tagged_t *get_tiny_blocks(void);
/*static char *mem_start; */  /* beginning of our virtual memory -- Shared */
extern int end;                             /* Does it exist in NeXT? (MCL) */

 /* total # bytes grabbed by malloc/realloc/free . Not accurately measured
    -- it depends on the mem. mang. own data structures (MCL) */
ENG_INT total_mem_count = 0;                                 /* Shared */

 /* # bytes used by the Prolog program & database code.  Probably not
    accurately measured (patched here and there) (MCL).  */
ENG_INT mem_prog_count = 0;                                     /* Shared */

 /* Number of predicates asserted */
ENG_INT num_of_predicates = 0;                                  /* Shared */

 /* locks for accessing the shared memory.  Although malloc() is MT-safe in
    Solaris (how 'bout Linux?), the engine uses several internal data
    structures for its own memory management which should be correctly
    updated.  So we use our own lock for providing an atomic access to
    tryalloc/tryrealloc/checkdealloc (MCL).
 */


extern SLOCK    mem_mng_l;

           /* From an execution profile, 24 seems a good threshhold (MCL) */
#define THRESHHOLD 24
#define NTINY 682

static tagged_t *tiny_blocks = NULL;                     /* Shared & locked */

static tagged_t *get_tiny_blocks()
{
  CIAO_REGISTER int i;
  tagged_t *ptr;
  CIAO_REGISTER tagged_t *p;
  tagged_t tail = 0;

 /* prt was a call to checkalloc, but I made a direct call to Malloc()
    because of recursive locks stopping the engine.  Thus, the total amount
    of memory is also increased here. */

  ptr = Malloc(NTINY*THRESHHOLD);

  if (!ptr) {
    Release_slock(mem_mng_l);
    MEMORY_FAULT("Memory allocation failed [Malloc()]");
  }
  if (!ENSURE_ADDRESSABLE(ptr, NTINY*THRESHHOLD)) {
    Release_slock(mem_mng_l);
    MEMORY_FAULT("Memory out of addressable bounds! [Malloc()]");
  }

  p = ptr+(NTINY*THRESHHOLD>>2);
  total_mem_count += NTINY*THRESHHOLD;

  for (i=NTINY; i>0; i--)
    p -= THRESHHOLD>>2, p[0] = tail, tail = (tagged_t)p;
  tiny_blocks = ptr+(THRESHHOLD>>2);
  return ptr;
}


tagged_t *tryalloc(size)
     int size;
{
  tagged_t *p;
  Wait_Acquire_slock(mem_mng_l);

  if (size<=THRESHHOLD) {
    if ((p=tiny_blocks))
      tiny_blocks = (tagged_t *)p[0];
    else {
      p=get_tiny_blocks();
      if (!p) {                                      /* get_tiny_block fails */
	Release_slock(mem_mng_l);
	return NULL;
      }
    }
  } else {                                           /* size > THRESHHOLD */
    p = (tagged_t *)Malloc(size);

    if (!p) {
      Release_slock(mem_mng_l);
      MEMORY_FAULT("Memory allocation failed [Malloc()]");
    }
    /* segfault patch -- jf */
    if (!ENSURE_ADDRESSABLE(p, size)) {
      Release_slock(mem_mng_l);
      MEMORY_FAULT("Memory out of addressable bounds! [Malloc()]");
    }

    /* mem_prog_count += size+sizeof(tagged_t); */
    total_mem_count += size;
  }
#if defined(DEBUG)
  if (debug_mem)
    printf("tryalloc returned %x, %d chars\n", (unsigned int)p, size);
#endif
  Release_slock(mem_mng_l);
  return p;
}

tagged_t *checkalloc(size)
     int size;
{
  CHECK_FOR_MEMORY_FAULT(tryalloc(size));
}

void checkdealloc(ptr,decr)
     tagged_t *ptr;
     int decr;
{
  Wait_Acquire_slock(mem_mng_l);

  if (decr<=THRESHHOLD) {
    ptr[0] = (tagged_t)tiny_blocks;
    tiny_blocks = ptr;
  } else {
    total_mem_count -= decr;
    Free(ptr);
  }
#if defined(DEBUG)
  if (debug_mem)
    printf("checkdealloc freed %x, %d chars\n", (unsigned int)ptr, decr);
#endif
  Release_slock(mem_mng_l);
}


tagged_t *tryrealloc(ptr,decr,size)
     tagged_t *ptr;
     int decr, size;
{
  tagged_t *p;

  Wait_Acquire_slock(mem_mng_l);

  if (decr<=THRESHHOLD) {
    if (size<=THRESHHOLD)
      p = ptr;
    else {
      p = Malloc(size);                       /* Was a call to checkalloc */
       if (!p) {
        Release_slock(mem_mng_l);
        MEMORY_FAULT("Memory allocation failed [Malloc()]");
      }
      /* segfault patch -- jf */
      if (!ENSURE_ADDRESSABLE(p, size)) {
	Release_slock(mem_mng_l);
	MEMORY_FAULT("Memory out of addressable bounds [Malloc()]");
      }
      total_mem_count += size;
      memcpy(p, ptr, ((decr-1) & -4)+4);
      ptr[0] = (tagged_t)tiny_blocks;
      tiny_blocks = ptr;
    }
  } else                                             /* decr > THRESHHOLD */
    if (size<=THRESHHOLD) {
      if (!(p=tiny_blocks)) {
	p=get_tiny_blocks(); 
	if (!p) {                                    /* get_tiny_block fails */
	  Release_slock(mem_mng_l);
	  return NULL;
	}
      }
      memcpy(p, ptr, ((size-1) & -4)+4);
    } else {
      p = (tagged_t *)Realloc(ptr,size);
      if (!p) {
        Release_slock(mem_mng_l);
        MEMORY_FAULT("Memory allocation failed [in Realloc()]");
      }
      /* segfault patch -- jf */
      if (!ENSURE_ADDRESSABLE(p, size)) {
	Release_slock(mem_mng_l);
	MEMORY_FAULT("Memory out of addressable bounds [Realloc()]");
      }
      /* mem_prog_count  += (size-decr); */
      total_mem_count += (size-decr);
    }
#if defined(DEBUG)
  if (debug_mem)
  printf("tryrealloc returned %x, %d chars\n",
         (unsigned int)p, size);
#endif
  Release_slock(mem_mng_l);
  return p;
}

tagged_t *checkrealloc(ptr,decr,size)
     tagged_t *ptr;
     int decr, size;
{
  CHECK_FOR_MEMORY_FAULT(tryrealloc(ptr,decr,size));
}


 /* Creates the wam structure, allocates its areas and initializes them.
    This returns an empty, fresh wam.  We do not add it here to the task
    state list; it needs its own thread, which we have after startwam() */


extern ENG_INT mem_prog_count;

worker_t *create_and_init_wam()
{
  Argdecl;
  /*ENG_INT saved_program_count = mem_prog_count;  */

  Arg = create_wam_storage();                         /* Just create *Arg */
  create_wam_areas(Arg);                      /* Make room for the stacks */
  numstack_init(Arg);                                     /* bignum areas */
  local_init_each_time(Arg);                               /* Local areas */
  return Arg;
}

/* Available workers are enqued here */

worker_t *wam_list = NULL;
SLOCK    wam_list_l;

worker_t *free_wam()
{
  worker_t *free_wam;

  Wait_Acquire_slock(wam_list_l);
  if (wam_list) {
    free_wam = wam_list;
    wam_list = Next_Worker(free_wam);
    Release_slock(wam_list_l);
    Next_Worker(free_wam) = NULL;
  } else {
    Release_slock(wam_list_l);
    free_wam = create_and_init_wam();
  }
  return free_wam;
}

void release_wam(wam)
     worker_t *wam;
{
  local_init_each_time(wam);
  Wait_Acquire_slock(wam_list_l);
  Next_Worker(wam) = wam_list;
  wam_list = wam;
  Release_slock(wam_list_l);
}


#if defined(ANDPARALLEL)
/* circular list of WAMs defined here */
worker_t *wam_circular_list = NULL;
worker_t *main_worker = NULL;
bool_t unwinding_done = FALSE;

/* lock for circular list of WAMs */
SLOCK wam_circular_list_l;

/* lock for expanding stacks */
SLOCK stackset_expansion_l;

/* procedure to add a new WAM to the circular list of WAMs */
void add_wam(worker_t *worker)
{
  Wait_Acquire_slock(wam_circular_list_l);
  if (wam_circular_list) {
    Next_Wam_Of(worker) = Next_Wam_Of(wam_circular_list);
    Next_Wam_Of(wam_circular_list) = worker;
    wam_circular_list = worker;
  }
  else {
    wam_circular_list = worker;
    Next_Wam_Of(worker) = worker;
    main_worker = worker;
  }
  Release_slock(wam_circular_list_l);
}
#endif

#if defined(PARBACK)
/* circular list of WAMs defined here */
worker_t *wam_circular_list = NULL;
worker_t *main_worker = NULL;

/* lock for circular list of WAMs */
SLOCK wam_circular_list_l;

/* lock for expanding stacks */
SLOCK stackset_expansion_l;

/* procedure to add a new WAM to the circular list of WAMs */
void add_wam(worker_t *worker)
{
  Wait_Acquire_slock(wam_circular_list_l);
  if (wam_circular_list) {
    Next_Wam_Of(worker) = Next_Wam_Of(wam_circular_list);
    Next_Wam_Of(wam_circular_list) = worker;
    wam_circular_list = worker;
  }
  else {
    wam_circular_list = worker;
    Next_Wam_Of(worker) = worker;
    main_worker = worker;
  }
  Release_slock(wam_circular_list_l);
}
#endif


#define CREATE_TYPED_STORAGE(T) (T *)checkalloc(sizeof(T))

worker_t *create_wam_storage() {
  worker_t *w;

  w = (worker_t *)checkalloc(SIZEOFWORKER(reg_bank_size));
  w->misc = CREATE_TYPED_STORAGE(misc_info_t);
  w->streams = CREATE_TYPED_STORAGE(io_streams_t);
  w->debugger_info = CREATE_TYPED_STORAGE(debugger_state_t);

  return w;
}


void create_wam_areas(Arg)
     Argdecl;
{
  int i, j;
  char *cp;

  Atom_Buffer = (char *)checkalloc(Atom_Buffer_Length=STATICMAXATOM);

#if defined(ANDPARALLEL)
  /* Initializing pointers and locks */
  Goal_List_Start = NULL;
  Goal_List_Top = NULL;
  Goal_Cache = NULL;
  Goal_Cache = NULL;
  Dep_Id = NULL;
  Dep_Size = 0;
  Event_Queue_Start = NULL;
  Event_Queue_Top = NULL;
  Last_Parallel_Exec = NULL;

  Suspended_Waiting_For_Work = FALSE;
  Cancel_Goal_Exec = FALSE;
  Safe_To_Cancel = TRUE;
  Suspend = RELEASED;
  Mode = FORWARD_EXEC;

  Init_slock(Goal_List_Lock);
  Init_slock(Event_Queue_Lock);
  Init_slock(Mutex_Lock);
  Init_lock(Waiting_For_Work_Lock);
  Cond_Var_Init(Waiting_For_Work_Cond_Var);

  /* Adding the new WAM to the circular list of WAMs */
  add_wam(w);

#if defined(VISANDOR)
  Pcall_Level(w) = 0;
  FirstEvent(w)  = (visandor_event_t *) checkalloc(maxevents * sizeof(visandor_event_t));
  NextEvent(w)   = FirstEvent(w);
  LastEvent(w)   = &((FirstEvent(w))[maxevents]);
#endif

#endif

#if defined(PARBACK)
  /* Initializing pointers and locks */
  Act_PF = NULL;
  Goal_List_Start = NULL;
  Goal_List_Top = NULL;
  Cancel_Goal_List_Start = NULL;
  Cancel_Goal_List_Top = NULL;
  Back_Goal_List_Start = NULL;
  Back_Goal_List_Top = NULL;

  Suspended_Waiting_For_Work = FALSE;
  Goal_To_Cancel = NULL;
  Safe_To_Cancel = TRUE;
  Suspend = RELEASED;

  Init_slock(Mutex_Lock);
  Init_lock(Waiting_For_Work_Lock);
  Cond_Var_Init(Waiting_For_Work_Cond_Var);

  /* Adding the new WAM to the circular list of WAMs */
  add_wam(w);
#endif

  /* heap pointer is first free cell, grows ++ */
  GETENV(i,cp,"GLOBALSTKSIZE",GLOBALSTKSIZE);
  Heap_Start = checkalloc(i*sizeof(tagged_t));
  Heap_End =  HeapOffset(Heap_Start,i);
  Heap_Warn_Soft =  Heap_Warn =  HeapOffset(Heap_End,-DEFAULT_SOFT_HEAPPAD);

#if defined(USE_OVERFLOW_EXCEPTIONS)
  SOFT_HEAPPAD = DEFAULT_SOFT_HEAPPAD;
  Heap_Limit = 0;
#endif 

  /* stack pointer is first free cell, grows ++ */
  GETENV(i,cp,"LOCALSTKSIZE",LOCALSTKSIZE);
  Stack_Start  = checkalloc(i*sizeof(tagged_t));
  Stack_End =  StackOffset(Stack_Start,i);
  Stack_Warn = StackOffset(Stack_End,-STACKPAD);

  /* trail pointer is first free cell, grows ++ */
  /* choice pointer is last busy cell, grows -- */
  GETENV(i,cp,"CHOICESTKSIZE",CHOICESTKSIZE);
  GETENV(j,cp,"TRAILSTKSIZE",TRAILSTKSIZE);
  i += j;
  Choice_End = Trail_Start = checkalloc(i*sizeof(tagged_t));
  Choice_Start =  Trail_End = TrailOffset(Trail_Start, i);


 /*  Do not touch the (tagged_t) type casting! Or the emulator will break! */

#if defined(USE_TAGGED_CHOICE_START)
  Tagged_Choice_Start = (tagged_t *)((tagged_t)Choice_Start + TaggedZero);
#endif
}


                             /* Global code */

void init_alloc()
{
  init_bootcode((bcp_t)checkalloc(sizeof(insn_t)*(6+BPTP)));
#if defined(INTERNAL_CALLING)
  init_internal_calling((bcp_t)checkalloc(sizeof(insn_t)*(6+BPTP)));
#endif
  init_startgoalcode((bcp_t)checkalloc(sizeof(insn_t)*(6+BPTP)));
  init_startgoalcode_cont((bcp_t)checkalloc(sizeof(insn_t)*(6+BPTP)));
  init_contcode((bcp_t)checkalloc(sizeof(insn_t)*(3*ARITYLIMIT)),
		(bcp_t)checkalloc(sizeof(insn_t)*(4+BPTP)));
}

/*  mem_start wrongly calculated, and mem_prog_count set to zero only once */
/*
void mem_prog_reset()
{
    mem_start = (char *)(&end);
#if MallocBase
  if (mem_start < (char *)MallocBase)
    mem_start = (char *)MallocBase;
#endif

  mem_prog_count = 0;
}
*/

/* program_usage: [sizeof_used_space, 0] */
bool_t program_usage(Arg)
     Argdecl;
{
  tagged_t x;

  MakeLST(x,TaggedZero,atom_nil);
  MakeLST(x,MakeInteger(Arg,mem_prog_count),x);
  return cunify(Arg,X(0),x);
}

/* internal_symbol_usage: [number_atoms_funcs_preds, number_pred_defs] */
bool_t internal_symbol_usage(Arg)
     Argdecl;
{
  tagged_t x;

  MakeLST(x,MakeInteger(Arg,num_of_predicates),atom_nil);
  MakeLST(x,MakeInteger(Arg,ciao_atoms->count),x);
  return cunify(Arg,X(0),x);
}


/* total_usage: [total_space, 0].  Changed to use total_mem_count (MCL) */
bool_t total_usage(Arg)
     Argdecl;
{
  tagged_t x;
  ENG_INT n;

/* n = (char *)sbrk(0) - mem_start; */
  n = total_mem_count;
  MakeLST(x,TaggedZero,atom_nil);
  MakeLST(x,MakeInteger(Arg,n),x);
  return cunify(Arg,X(0),x);
}


bool_t statistics(Arg)
     Argdecl;
{
  stream_node_t *s = Output_Stream_Ptr;
  ENG_INT used, free;
  ENG_LINT runtick0;
  ENG_LINT usertick0 = usertick();
  ENG_LINT systemtick0 = systemtick();
  ENG_LINT walltick0 = walltick();
  frame_t *newa;

  runtick0=usertick0;

  /*
  ENG_PRINTF1(s,
             "memory (total)    %10ld bytes\n",
             (long int)((char *)sbrk(0)-mem_start));
  */
  ENG_PRINTF1(s,
             "memory used (total)    %10ld bytes\n",
             (long int)total_mem_count);
  ENG_PRINTF1(s, 
              "   program space (including reserved for atoms): %ld bytes\n", 
              mem_prog_count);

  ENG_PRINTF1(s,
              "   number of atoms and functor/predicate names: %ld\n", 
              ciao_atoms->count);
  ENG_PRINTF1(s,
              "   number of predicate definitions: %ld\n", 
              num_of_predicates);

  used = HeapCharDifference(Heap_Start,w->global_top);
  free = HeapCharDifference(w->global_top,Heap_End);
  ENG_PRINTF3(s, 
              "   global stack   %10ld bytes:%10ld in use,%10ld free\n",
              used+free, used, free);

  ComputeA(newa,w->node);
  used = StackCharDifference(Stack_Start,newa);
  free = StackCharDifference(newa,Stack_End);
  ENG_PRINTF3(s,
              "   local stack    %10ld bytes:%10ld in use,%10ld free\n",
              used+free, used, free);

  used = TrailCharDifference(Trail_Start,w->trail_top);
  free = TrailCharDifference(w->trail_top,w->node)/2;
  ENG_PRINTF3(s,
              "   trail stack    %10ld bytes:%10ld in use,%10ld free\n",
              used+free, used, free);

  used = ChoiceCharDifference(Choice_Start,w->node);
  free = ChoiceCharDifference(w->node,w->trail_top)/2;
  ENG_PRINTF3(s,
              "   control stack  %10ld bytes:%10ld in use,%10ld free\n\n",
              used+free, used, free);

  ENG_PRINTF4(s,
     " %10.6f sec. for %ld global, %ld local, and %ld control space overflows\n",
              ((ENG_FLT)ciao_statistics.ss_tick)/GET_CLOCKFREQ(ciao_statistics),
              ciao_statistics.ss_global,
	      ciao_statistics.ss_local, ciao_statistics.ss_control);
  ENG_PRINTF3(s,
     " %10.6f sec. for %ld garbage collections which collected %ld bytes\n\n",
              ((ENG_FLT)ciao_statistics.gc_tick)/GET_CLOCKFREQ(ciao_statistics),
              ciao_statistics.gc_count,
	      ciao_statistics.gc_acc*sizeof(tagged_t));

  ENG_PRINTF3(s,
      " runtime:    %10.6f sec. %12lld ticks at %12lld Hz\n",
     (ENG_FLT)(runtick0-ciao_statistics.starttick)/
              GET_CLOCKFREQ(ciao_statistics),
              runtick0-ciao_statistics.starttick,
	      GET_CLOCKFREQ(ciao_statistics));
  ENG_PRINTF3(s,
              " usertime:   %10.6f sec. %12lld ticks at %12lld Hz\n",
	      (ENG_FLT)(usertick0-ciao_statistics.startusertick)/
                       ciao_statistics.userclockfreq,
	      usertick0-ciao_statistics.startusertick,
	      ciao_statistics.userclockfreq);
  ENG_PRINTF3(s,
              " systemtime: %10.6f sec. %12lld ticks at %12lld Hz\n",
	      (ENG_FLT)(systemtick0-ciao_statistics.startsystemtick)/
                       ciao_statistics.systemclockfreq,
              systemtick0-ciao_statistics.startsystemtick,
	      ciao_statistics.systemclockfreq);

  ENG_PRINTF3(s,
              " walltime:   %10.6f sec. %12lld ticks at %12lld Hz\n\n",
	      (ENG_FLT)(walltick0-ciao_statistics.startwalltick)/
                        ciao_statistics.wallclockfreq,
	      walltick0-ciao_statistics.startwalltick,
	      ciao_statistics.wallclockfreq);

  return TRUE;
}
