/* (C) 1997 The CLIP Group */

/* Workers can be: */

typedef enum {
  IDLE,      /* The memory areas are available for being used by a thread */
  WORKING,                 /* The memory areas are being used by a thread */
  WAITING                              /* Frozen -- maybe on backtracking */
} Thread_State;



/* Save wam() local variables, to reenter after leaving it blocked (e.g., if
   waiting for backtracking). */

struct wam_private {
  INSN *p;		                               /* program counter */
  int i;
  TAGGED *pt1, *pt2, t0, t1, t2, t3;
  INSN *ptemp;
  int wam_exit_code;
  struct instance *ins;	
};


/* Possible actions requested from the toplevel. */

#define NO_ACTION         0
#define SHARES_STRUCTURE  1
#define HAS_CONTINUATION  2
#define KEEP_STACKS       4
#define BACKTRACKING      8

/* This is the state of the worker; they are held together in a linked list.
   A worker is free iff its state is IDLE.  The wam_private_state is only
   meaningful when it is in a WAITING state.  The rest of the fields just
   reflect the corresponding values in the worker_entry of the goal. */

struct wrb_state_str {
  struct worker *worker_registers;                   /* The WAM registers */
  THREAD_T thread_id;                               /* Thread Id (cached) */
  int      goal_id;                            /* Entry in array (cached) */
  int      action;                                 /* What to do (cached) */
  Thread_State state;                                  /* What I am up to */
  struct wam_private wam_private_state;
  struct wrb_state_str *next;
};

typedef struct wrb_state_str wrb_state;
typedef struct wrb_state_str *wrb_state_p;


/* This is a preliminary state of a worker.  It is filled with minimal
   information when a thread requests the creation of another thread.  Then
   the generated thread looks for memory areas to execute, and fills in the
   remaining area. */

struct conts {
  TAGGED goal_and_success;                                        /* goal */
  TAGGED on_failure;                              /* Failure continuation */
};


/* Wether a worker entry is free or not is denoted exclusively by the
   "worker" pointer being NULL or not */

typedef struct {
  union {
    TAGGED goal;
    struct conts gas;
  } g;
  TAGGED goal_or_goal_and_conts;                   /* Depending on action */
  TAGGED goal;
  THREAD_T thread_id;                  /* Filled in when launching thread */
  int goal_id;                                  /* Id in the worker table */
  int action;
  BOOL create_thread;
  wrb_state_p worker;
} worker_entry, *worker_entry_p;
