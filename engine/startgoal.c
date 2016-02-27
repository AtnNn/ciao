/* Copyright (C) 1996,1997,1998, UPM-CLIP */

#include <setjmp.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

#include "datadefs.h"
#include "support.h"
#include "wam.h"
#include "compat.h"
#include "threads.h"
#include "task_areas.h"

#include "initial_defs.h"
#include "inout_defs.h"
#include "main_defs.h"
#include "prolog_tasks_defs.h"
#include "startgoal_defs.h"
#include "tasks_defs.h"
#include "term_support_defs.h"
#include "wam_defs.h"
#if defined(DEBUG)
#include "locks_defs.h"
#endif


/* private function declarations */

worker_entry_p find_a_new_worker(void);

/* Here with w->next_insn set up -- see local_init_each_time(). (MCL) */

JMP_BUF abort_env;                                              /* Shared */


void firstgoal(worker, goal_name)
     wrb_state_p  worker;
     char        *goal_name;
{
  int i, exit_code;
  Argdecl;

  Arg = worker->worker_registers;
  Arg->node->term[0] = X(0) = init_atom_check(goal_name);
  Arg->next_insn = bootcode;

  while(TRUE) {
    i = SETJMP(abort_env);
    if (i == 0){                /* Just made longjmp */
      Arg->term[0] = Arg->node->term[0];
      wam_initialized = TRUE;
      exit_code = wam(Arg, worker);
      flush_output(Arg);
      if (exit_code != WAM_ABORT) /* halting... */
        break;
    }
    else if (i == -1) {         /* SIGINT during I/O */
      REGISTER TAGGED *pt1;
      /* No need to patch "p" here, since we are not exiting wam() */
      REGISTER INSN *p = (INSN *)int_address;
      int_address = NULL;
      SETUP_PENDING_CALL(address_true);
      continue;
    }
#if defined(THREADS)
    (void)prolog_kill_other_threads(Arg);
#endif 
    reinitialize(Arg);                                      /* aborting... */
    init_each_time(Arg);                /* Sets X(0) to point to bootcode */
    *(struct definition **)(bootcode+2) = address_restart;
  }
  at_exit(exit_code);
}


/* Returns a free worker.  If needed, create memory areas.  Initialize
   registers, etc. The worker is already marked as WORKING to avoid other
   threads stealing it.  The WAM areas are already initialized. */

wrb_state_p gimme_a_new_worker(wrk_entry)
     worker_entry_p wrk_entry;
{
  wrb_state_p worker_to_run;

  if ((worker_to_run = look_for_a_free_worker()) == NULL) {
    worker_to_run = attach_me_to_wrb_state_list(create_and_init_wam());
  }

  worker_to_run->thread_id = wrk_entry->thread_id;
  worker_to_run->goal_id   = wrk_entry->goal_id;
  worker_to_run->action    = wrk_entry->action;
  wrk_entry->worker     = worker_to_run;
  return worker_to_run;
}

void *startgoal(wo)
     void *wo;
{
  Argdecl;
  int exit_code;
  wrb_state_p  worker_to_run;
  worker_entry_p worker_entry = (worker_entry_p)wo;

#if defined(DEBUG) && defined(THREADS)
  if (debug_threads) printf("New thread, goal %d, actions %d\n", 
                            worker_entry->goal_id,
                            worker_entry->action);
#endif

  disallow_thread_cancellation();     /* I should have my own thread here */
  worker_to_run = gimme_a_new_worker(worker_entry);
  /* This is to check whether backtracking yielded a new solution */
  worker_to_run->node_at_entry =
    worker_to_run->worker_registers->node;

#if defined(DEBUG) && defined(THREADS)
  if (debug_threads) printf("New worker for goal %d\n", worker_entry->goal_id);
#endif

  Arg = worker_to_run->worker_registers;

  if (worker_entry->action & SHARES_STRUCTURE)
    X(0) = (TAGGED)worker_entry->goal_or_goal_and_conts;
  else
    DEREF(X(0), cross_copy_term(Arg, worker_entry->goal_or_goal_and_conts));
  Arg->next_insn = 
    worker_entry->action & HAS_CONTINUATION ?
      startgoalcode_cont : 
      startgoalcode;
  Arg->node->term[0] = X(0);
  allow_thread_cancellation();        

  Release_lock(launch_goal_l);

#if defined(DEBUG) && defined(THREADS)
  if (debug_threads)
    printf("Goal %d entering wam()\n", worker_entry->goal_id);
#endif
  flush_output(Arg);

  exit_code = wam(Arg, worker_entry->worker);

  /* If ID was requested, freeze the worker; otherwise make it available to
     other threads */

  if (worker_entry->action & KEEP_STACKS) 
    worker_entry->worker->state = WAITING;
  else 
    make_worker_entry_free(worker_entry);

#if defined(DEBUG) && defined(THREADS)
  if (debug_threads || debug_conc)
    printf("*** %d(%d) Goal %d is EXITING\n", 
           (int)Thread_Id, (int)GET_INC_COUNTER, 
           worker_entry->goal_id);
#endif

  return NULL;                              /*  Avoid compiler complaints */
}




void *startgoal_simp(wo)
     void *wo;
{
  Argdecl;
  int exit_code;
  wrb_state_p  worker_to_run;
  worker_entry_p worker_entry = (worker_entry_p)wo;

#if defined(DEBUG) && defined(THREADS)
  if (debug_threads) printf("New goal, goal %d, actions %d\n", 
                            worker_entry->goal_id,
                            worker_entry->action);
#endif

  disallow_thread_cancellation();     /* I should have my own thread here */

  worker_to_run = worker_entry->worker;
  worker_entry->thread_id = worker_to_run->thread_id = Thread_Id;

  /* This helps in checking whether backtracking (or the first execution
     itself) yielded a new solution */
  worker_to_run->node_at_entry = worker_to_run->worker_registers->node;

#if defined(DEBUG) && defined(THREADS)
  if (debug_threads) printf("New worker for goal %d\n", worker_entry->goal_id);
#endif

  Arg = worker_to_run->worker_registers;

  /* No needed: already copied in the thread which launches the goal */
  /* DEREF(X(0), cross_copy_term(Arg, worker_entry->goal_or_goal_and_conts));*/
  Arg->next_insn = startgoalcode;
  Arg->node->term[0] = X(0);
  allow_thread_cancellation();        

  if (worker_entry->create_thread)
    Release_lock(launch_goal_l);
#if defined(DEBUG) && defined(THREADS)
  if (debug_threads)
    printf("Goal %d entering wam()\n", worker_entry->goal_id);
#endif
  flush_output(Arg);

  exit_code = wam(Arg, worker_entry->worker);

  if (worker_entry->action & KEEP_STACKS){
    worker_entry->worker->state = WAITING;           /* Freeze the worker */
  }
  else{                     /* We do not want the worker for anything else */
    make_worker_entry_free(wo);
  }
#if defined(DEBUG) && defined(THREADS)
  if (debug_threads || debug_conc)
    printf("*** %d(%d) Goal %d is EXITING\n", 
           (int)Thread_Id, (int)GET_INC_COUNTER, 
           worker_entry->goal_id);
#endif

  return NULL;                              /*  Avoid compiler complaints */
}



