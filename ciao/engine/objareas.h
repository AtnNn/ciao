/* Copyright (C) 1988,1989 Swedish Institute of Computer Science and The
   Aerospace Corporation. */

/* In registers.c */
extern int reg_bank_size;
extern char **prolog_argv;
extern int prolog_argc;
/*extern worker_t *self;*/

typedef struct instance_handle_ instance_handle_t;
typedef struct instance_ instance_t;
typedef struct int_info_ int_info_t;
typedef struct sw_on_key_ sw_on_key_t;

/* OBJECT AREA ----------------------------------------------------*/ 

#include <stdio.h>                       /* Needed for the FILE structure */

#define CACHE_INCREMENTAL_CLAUSE_INSERTION

typedef unsigned short int instance_clock_t;

#define ACTIVE_INSTANCE(ARG,I,TIME,CHAINP) \
  ((I)==NULL ? NULL : \
   ((TIME) >= (I)->birth && (TIME) < (I)->death) ? (I) : \
   active_instance(ARG,I,TIME,CHAINP))


 /* This one does not look at time creation of clauses. */

#define ACTIVE_INSTANCE_conc(ARG,I,ROOT)  \
     active_instance_conc(ARG,I,ROOT) 

extern instance_t 
  *active_instance PROTO((Argdecl, instance_t *i,int time,bool_t normal));


/* p->count = (ENG_INT *)((char *)p + objsize) - p->counters */

#define NumberOfCounters(cl) \
  ((ENG_INT *)((char *)cl + cl->objsize) - cl->counters)


 /* Clauses of compiled predicates are stored as a linked list of
    records. The forward link of the last clause contains the total number
    of clauses. */

typedef struct emul_info_ emul_info_t;
struct emul_info_ {
	emul_info_t *next;          /* next clause OR no. of clauses */
	definition_t *subdefs;
	int objsize;		                         /* total # chars */
#if defined(GAUGE)
	ENG_INT *counters;	    /* Pointer to clause's first counter. */
#endif
	insn_t emulcode[ANY];
	};


/* All invocations looking at an instance of an concurrent predicate will
   actually have a pointer to a pointer to the instance.  Every clause has a
   pointer to a queue of handles to itself.  Erasing a pointed to instance 
   will change the pointer to the instance itself (Confused? OK, ask me, I
   have a nice drawing on a napkin --- MCL) */

struct instance_handle_ {
  instance_t *inst_ptr;                    /* Pointer to the instance */
  tagged_t head;                            /* Goal called; allows indexing. */
  instance_handle_t *next_handle;             
  instance_handle_t *previous_handle;             
};

/* Terms recorded under a key or clauses of an interpreted predicate
   are stored as a doubly linked list of records.  The forward link is
   terminated by NULL; the backward link wraps around.  Each instance
   points back to the beginning of the list.  The rank field defines a
   total order on a list.  Two timestamps are associated with each
   instance to support proper semantics for dynamic code
   updates. (MCL, with help from the manual).  There are also two
   pointers (one for unindexed and other for indexed accesses) to
   queues which maintain the list of calls looking at each
   instance. */

struct instance_ {
  instance_t *forward;
  instance_t *backward;
  int_info_t *root;
  instance_t *next_forward;
  instance_t *next_backward;
  tagged_t key;
  tagged_t rank;
  instance_clock_t birth, death;                          /* Dynamic clause lifespan */

  instance_handle_t *pending_x2;       /* Seen by invocations looking @ here */
  instance_handle_t *pending_x5;       /* Seen by invocations looking @ here */

  int objsize;		                                 /* total # chars */
  insn_t emulcode[ANY];
};

/* Indexing information for dynamic predicates? First simple, common cases,
   then hash table indexing.  Includes information on openness/closeness of
   concurrent facts. (MCL) */

typedef enum {DYNAMIC, CONC_OPEN, CONC_CLOSED} int_behavior_t;

struct int_info_ {
  volatile int_behavior_t  behavior_on_failure;/* behavior if no clauses match. */


#if defined(CONDITIONAL_VARS)
  condition_t clause_insertion_cond;
#else
  /*  SLOCK clause_insertion_cond;            */
  condition_t clause_insertion_cond;
#endif

  instance_handle_t *x2_pending_on_instance;     /* Used when pred. is empty */
  instance_handle_t *x5_pending_on_instance;

  instance_t  *first  ;
  instance_t  *varcase;
  instance_t  *lstcase;
  sw_on_key_t *indexer;
};

typedef struct und_info_ und_info_t;
struct und_info_ {
  int unused;
};

typedef bool_t   (*CInfo)();
typedef tagged_t (*TInfo)();

extern stream_node_t *root_stream_ptr;

 /* Information about atoms */

typedef struct atom_ atom_t;
struct atom_ {        
  unsigned int has_squote:1;
  unsigned int has_dquote:1;
  unsigned int has_special:1;
  unsigned int index:29;
                               /* support for locking on atom names. MCL. */
#if defined(THREADS)                    /* Do not waste space otherwise */
  LOCK atom_lock_l;                      /* May be held for a long time */
#if defined(GENERAL_LOCKS)
  volatile int atom_lock_counter;               /* For general semaphores */
  SLOCK counter_lock;                            /* Held for a short time */
#endif
#endif
  unsigned int atom_len;
  char name[ANY];
};



/* For a given goal and an emulated predicate, alternatives that might match
   the goal are stored as a linked list of records */

/* try-retry-trust repr. as a linked list. */

struct try_node_ {
  try_node_t *next;                      /* Next alternative or NULL */
  bcp_t emul_p;                    /* write mode or not first alternative */
  bcp_t emul_p2;		          /* read mode, first alternative */
  short node_offset;		       /* offset from choicepoint to next */
  /*short number;*/
  unsigned int number;		    /* clause number for this alternative */
                             /* Gauge specific fields MUST come after this*/
#if defined(GAUGE)
  ENG_INT *entry_counter;        /* Offset of counter for clause entry */
#endif
  };

#define ArityToOffset(A)  \
  (((A)+(sizeof(node_t)/sizeof(tagged_t)-ANY))<<2)
#define OffsetToArity(O) \
  (((O)>>2)-(sizeof(node_t)/sizeof(tagged_t)-ANY))

/* switch_on_constant/structure: hash on terms. */

/* 
    4 is log2(sizeof(sw_on_key_node_t))

    IF THE SIZE OF "sw_on_key_node_t" CHANGES, THIS CONSTANT MUST BE
    UPDATED AS WELL.  Besides, THE SIZE OF "sw_on_key_node_t" MUST BE A
    POWER OF 2.

    (MCL, after experimentation and inspection)

*/

 /* Remember to update if sw_on_key_node ever changes*/

#define LOG_SWITCH_KEY 3


#define SwitchSize(X) (((X)->mask>>LOG_SWITCH_KEY)+1) 
#define SizeToMask(X) (((X)-1)<<LOG_SWITCH_KEY)

/* REMEMBER TO UPDATE THE CONSTANT ABOVE IF THIS STRUCT CHANGES
   THE SIZE OF THE STRUCT BELOW MUST BE A POWER OF TWO --- ADD PADDING IF
   NECESSARY  */

typedef struct sw_on_key_node_ sw_on_key_node_t;
struct sw_on_key_node_ {
  tagged_t key;
  union {
    try_node_t *try_chain;     /* try-retry-trust as linked list */
    instance_t *instp;             /* int. clauses or recorded terms */
    sw_on_key_node_t *node;
    definition_t *def;                       /* predicate definition */
    int_info_t *irootp;                             /* indexer info? */
    atom_t *atomp;               /* Info on atoms and main functors  */
    TInfo tinfo;                                                   /* ??? */
    CInfo cinfo;                                   /* C function pointer? */
  } value;
};


/* Indexing table are used in indexing on first argument in calls and in
   looking up predicates.  They are operated as hash tables with quadratic
   overflow handling.  Hash table access is performed by using some of the
   low order bits of the key as array index, and then searching for a hit or
   for a zero key indicating that the key is absent from the table. 
   
   MCL: changed to make room for erased atoms: now a key == 1 indicates
   that the entry has been erased (but the search chain continues).  New 
   atoms are placed in the first free entry.

   NOTE (MCL): data structure different from that in Sicstus 1.8!!
*/

struct sw_on_key_ {
  unsigned long mask;                                          /* bitmask */
  long int count;
#if defined(ATOMGC)
  long int next_index;
#endif
  union {
    char aschar[ANY];
    sw_on_key_node_t asnode[ANY];
  } tab;
};

typedef struct incore_info_ incore_info_t;
struct incore_info_ {
  emul_info_t *clauses;	                          /* first clause */
  emul_info_t **clauses_tail;         /* "next" field of last clause */
#if defined(CACHE_INCREMENTAL_CLAUSE_INSERTION)
  emul_info_t *last_inserted_clause;   /* Pointer to the last clause */
  unsigned int last_inserted_num;           /* Number of last ins. clause */
#endif
  try_node_t *varcase;
  try_node_t *lstcase;
  sw_on_key_t *othercase;
};

#define SetEnterInstr(F,I) \
{ \
  (F)->predtyp = (I); \
  (F)->enter_instr = \
    (F)->properties.spy ? SPYPOINT : \
      (F)->properties.wait ? WAITPOINT : \
	(F)->properties.breakp ? BREAKPOINT : \
	  (F)->predtyp; \
}

typedef union definfo_ definfo_t;
union definfo_ {
  int_info_t *intinfo;
  und_info_t *undinfo;
  incore_info_t *incoreinfo;
#if 0 /* was GAUGE */
  c_code_info_t *cinfo;
#else
  CInfo cinfo;
#endif
};

struct definition_ {
  short enter_instr;	                                 /* see predtyp.h */
  short arity; /*  */
  tagged_t printname;	                        /* or sibling pointer | 1 */
                                                /* or parent pointer | 3 */
  struct {
    unsigned int spy:1;
    unsigned int breakp:1;
    /* unsigned int public:1; */
                      /* concurrent obeys declaration and implies dynamic */
    unsigned int concurrent:1;      /* 1 if wait on it, 0 if closed (MCL) */
    unsigned int wait:1;                             /* obeys declaration */
    unsigned int dynamic:1;                                       /* -""- */
    unsigned int multifile:1;                                     /* -""- */
    unsigned int nonvar:1;             /* seen P(X,...) :- var(X), !, ... */
    unsigned int var:1;             /* seen P(X,...) :- nonvar(X), !, ... */
  } properties;
  short predtyp;
  definfo_t code;
};

#define DEF_SIBLING(F) \
  ((F)->printname&2 ? NULL : (definition_t *)TagToPointer((F)->printname))

/* Classified somewhere else */
extern sw_on_key_node_t **atmtab;
extern sw_on_key_t *ciao_atoms;
extern CInfo builtintab[];

typedef struct statistics_ statistics_t;
struct statistics_ {
  ENG_LINT ss_tick;		             /* time spent stack_shifting */
  ENG_INT ss_global;		                       /* # global shifts */
  ENG_INT ss_local;		                       /* # local shifts  */
  ENG_INT ss_control;	                        /* # control/trail shifts */
  ENG_LINT gc_tick;		                 /* Total GC ticks (sec) */
  ENG_INT gc_count;		                 /* # garbage collections */
  ENG_INT gc_acc;		            /* Total reclaimed heap space */

  ENG_LINT starttick;
  ENG_LINT lasttick;

  ENG_LINT startwalltick;
  ENG_LINT lastwalltick;
  ENG_LINT wallclockfreq;

  ENG_LINT startusertick;
  ENG_LINT lastusertick;
  ENG_LINT userclockfreq;

  ENG_LINT startsystemtick;
  ENG_LINT lastsystemtick;
  ENG_LINT systemclockfreq;
};

extern statistics_t ciao_statistics; /* Shared, I guess */


#if defined(GAUGE)

typedef struct c_code_info_ c_code_info_t;
struct c_code_info_ {
  CInfo procedure; 
  ENG_INT counter;
};

#define INCR_COUNTER(c)   ((*(c))++)
#define INCR_ENTRY_COUNTER(c)   INCR_COUNTER(&c)

#endif

typedef struct other_stuff_ other_stuff_t;
struct other_stuff_ {     /* Usable type for passing data areas to gc etc */
  char *pointer;
  int size;
};


/* To handle arrays of ql code as files. MCL. */

typedef struct qlstream_ qlstream_t;
struct qlstream_ {
  char *qlpointer;
  int qlremains;
};

