/* Copyright (C) 1988,1989 Swedish Institute of Computer Science and The
   Aerospace Corporation. */

/* OBJECT AREA ----------------------------------------------------*/ 

#include <stdio.h>                       /* Needed for the FILE structure */

typedef unsigned short int CLOCK;


#define ACTIVE_INSTANCE(ARG,I,TIME,CHAINP) \
  ((I)==NULL ? NULL : \
   ((TIME) >= (I)->birth && (TIME) < (I)->death) ? (I) : \
   active_instance(ARG,I,TIME,CHAINP))


 /* This one does not look at time creation of clauses. NT comes from "No
    Time" (MCL) */

#define ACTIVE_INSTANCE_conc(ARG,I,ROOT)  \
     active_instance_conc(ARG,I,ROOT) 

extern struct instance 
  *active_instance PROTO((Argdecl, struct instance *i,int time,BOOL normal));


/* p->count = (ENG_INT *)((char *)p + objsize) - p->counters */

#define NumberOfCounters(cl) \
  ((ENG_INT *)((char *)cl + cl->objsize) - cl->counters)


 /* Clauses of compiled predicates are stored as a linked list of
    records. The forward link of the last clause contains the total number
    of clauses. */

struct emul_info {
	struct emul_info *next;          /* next clause OR no. of clauses */
	struct definition *subdefs;
	int objsize;		                         /* total # chars */
#if defined(GAUGE)
	ENG_INT *counters;	    /* Pointer to clause's first counter. */
#endif
	INSN emulcode[ANY];
	};


/* All invocations looking at an instance of an concurrent predicate will
   actually have a pointer to a pointer to the instance.  Every clause has a
   pointer to a queue of handles to itself.  Erasing a pointed to instance 
   will change the pointer to the instance itself (Confused? OK, ask me, I
   have a nice drawing on a napkin --- MCL) */

typedef struct instance_handle {
  struct instance *inst_ptr;                    /* Pointer to the instance */
  TAGGED head;                            /* Goal called; allows indexing. */
  struct instance_handle *next_handle;             
  struct instance_handle *previous_handle;             
} InstanceHandle;


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

struct instance {
  struct instance *forward;
  struct instance *backward;
  struct int_info *root;
  struct instance *next_forward;
  struct instance *next_backward;
  TAGGED key;
  TAGGED rank;
  CLOCK birth, death;                          /* Dynamic clause lifespan */

  InstanceHandle *pending_x2;       /* Seen by invocations looking @ here */
  InstanceHandle *pending_x5;       /* Seen by invocations looking @ here */

  int objsize;		                                 /* total # chars */
  INSN emulcode[ANY];
};

/* Indexing information for dynamic predicates? First simple, common cases,
   then hash table indexing.  Includes information on openness/closeness of
   concurrent facts. (MCL) */

typedef enum {DYNAMIC, CONC_OPEN, CONC_CLOSED} Behavior;

struct int_info {
  Behavior  behavior_on_failure;         /* behavior if no clauses match. */

  LOCK clause_lock_l;

  InstanceHandle *x2_pending_on_instance;     /* Used when pred. is empty */
  InstanceHandle *x5_pending_on_instance;

  struct instance *first;
  struct instance *varcase;
  struct instance *lstcase;
  struct sw_on_key *indexer;
};

struct und_info {
	int unused;
	};

typedef BOOL (*CInfo)();
typedef TAGGED (*TInfo)();


                                /* Streams: a doubly linked circular list */

struct stream_node {
  TAGGED label;
  struct stream_node *backward;
  struct stream_node *forward;
  TAGGED streamname;
  char streammode;
  int pending_char;                                      /* From peek'ing */
  unsigned int isatty:1;
  unsigned int socket_eof:1;
  ENG_INT last_nl_pos;
  ENG_INT nl_count;
  ENG_INT char_count;
  FILE *streamfile;                               /* Not used for sockets */
};

extern struct stream_node *root_stream_ptr;

 /* Information about atoms */

struct atom {        
  unsigned int has_squote:1;
  unsigned int has_dquote:1;
  unsigned int has_special:1;
  unsigned int index:29;
                               /* support for locking on atom names. MCL. */
#if defined(THREADS)                    /* Do not waste space otherwise */
  LOCK atom_lock_l;   /* Storage in separate area at object time creation */
#if defined(GENERAL_LOCKS)
  volatile int atom_lock_counter;               /* For general semaphores */
#endif
#endif
  char name[ANY];
};



/* For a given goal and an emulated predicate, alternatives that might match
   the goal are stored as a linked list of records */

/* try-retry-trust repr. as a linked list. */

struct try_node {
  struct try_node *next;                      /* Next alternative or NULL */
  INSN *emul_p;                    /* write mode or not first alternative */
  INSN *emul_p2;		          /* read mode, first alternative */
  short node_offset;		       /* offset from choicepoint to next */
  short number;			    /* clause number for this alternative */
                             /* Gauge specific fields MUST come after this*/
#if defined(GAUGE)
  ENG_INT *entry_counter;        /* Offset of counter for clause entry */
#endif
  };

#define ArityToOffset(A)  \
  (((A)+(sizeof(struct node)/sizeof(TAGGED)-ANY))<<2)
#define OffsetToArity(O) \
  (((O)>>2)-(sizeof(struct node)/sizeof(TAGGED)-ANY))

/* switch_on_constant/structure: hash on terms. */

#define SwitchSize(X)	(((X)->mask>>3)+1) /* 3 is log2(sizeof(struct sw_on_key_node)) */
#define SizeToMask(X)	(((X)-1)<<3)

struct sw_on_key_node {
  TAGGED key;
  union {
    struct try_node *try;               /* try-retry-trust as linked list */
    struct instance *instp;             /* int. clauses or recorded terms */
    struct sw_on_key_node *node;
    struct definition *def;                       /* predicate definition */
    struct int_info *irootp;                             /* indexer info? */
    struct atom *atomp;               /* Info on atoms and main functors  */
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

struct sw_on_key {
  unsigned long mask;                                          /* bitmask */
  long int count;
#if defined(ATOMGC)
  long int next_index;
#endif
  union {
    char aschar[ANY];
    struct sw_on_key_node asnode[ANY];
  } tab;
};

struct incore_info {
  struct emul_info *clauses;	                          /* first clause */
  struct emul_info **clauses_tail;         /* where to insert next clause */
  struct try_node *varcase;
  struct try_node *lstcase;
  struct sw_on_key *othercase;
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

union definfo {
  struct int_info *intinfo;
  struct und_info *undinfo;
  struct incore_info *incoreinfo;
#if 0 /* was GAUGE */
  struct c_code_info *cinfo;
#else
  CInfo cinfo;
#endif
};



struct definition {
  short enter_instr;	                                 /* see predtyp.h */
  short arity; /*  */
  TAGGED printname;	                        /* or sibling pointer | 1 */
                                                 /* or parent pointer | 3 */
                               /* support for locking on atom names. MCL. */
#if defined(PROFILE) 
  unsigned long int no_of_calls;
  unsigned long int time_spent;
#endif

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
  union definfo code;
};

#define DEF_SIBLING(F) \
  ((F)->printname&2 ? NULL : (struct definition *)TagToPointer((F)->printname))

/* Classified somewhere else */
extern struct sw_on_key_node **atmtab;
extern struct sw_on_key *prolog_atoms;
extern CInfo builtintab[];

struct statistics {
  ENG_FLT ss_time;		             /* time spent stack_shifting */
  ENG_INT ss_global;		                       /* # global shifts */
  ENG_INT ss_local;		                       /* # local shifts  */
  ENG_INT ss_control;	                        /* # control/trail shifts */
  ENG_FLT gc_time;		                   /* Total GC time (sec) */
  ENG_INT gc_count;		                 /* # garbage collections */
  ENG_INT gc_acc;		            /* Total reclaimed heap space */
  ENG_FLT starttime;
  ENG_FLT lasttime;
  ENG_FLT startwalltime;
  ENG_FLT lastwalltime;
};

extern struct statistics stats;                        /* Shared, I guess */


#if defined(GAUGE)

struct c_code_info {
  CInfo procedure; 
  ENG_INT counter;
};

#define INCR_COUNTER(c)   ((*(c))++)
#define INCR_ENTRY_COUNTER(c)   INCR_COUNTER(&c)

#endif

struct other_stuff {     /* Usable type for passing data areas to gc etc */
  char *pointer;
  int size;
};


/* To handle arrays of ql code as files. MCL. */

struct qlstream {
  char *qlpointer;
  int qlremains;
};

