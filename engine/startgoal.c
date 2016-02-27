/* Copyright (C) 1996,1997,1998, UPM-CLIP */

/* Change the name of this file to rungoals.c */

#include <setjmp.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

#include "threads.h"
#include "datadefs.h"
#include "support.h"
#include "wam.h"
#include "compat.h"
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


/* Here with w->next_insn set up -- see local_init_each_time(). (MCL) */

JMP_BUF abort_env;                                              /* Shared */


void firstgoal(goal_desc, goal_name)
     goal_descriptor_p  goal_desc;
     char              *goal_name;
{
  int i, exit_code;
  Argdecl;

  Arg = goal_desc->worker_registers;
  Arg->node->term[0] = X(0) = init_atom_check(goal_name);
  Arg->next_insn = bootcode;

  while(TRUE) {
    i = SETJMP(abort_env);
    if (i == 0){                /* Just made longjmp */
      Arg->term[0] = Arg->node->term[0];
      wam_initialized = TRUE;
      exit_code = wam(Arg, goal_desc);
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
    (void)prolog_eng_killothers(Arg);
#endif 
    reinitialize(Arg);                                      /* aborting... */
    init_each_time(Arg);                /* Sets X(0) to point to bootcode */
    *(struct definition **)(bootcode+2) = address_restart;
  }
  at_exit(exit_code);
}


THREAD_RES_T startgoal(wo)
     THREAD_ARG wo;
{
  Argdecl;
  goal_descriptor_p goal_desc = (goal_descriptor_p)wo;
  int result_state;
  int wam_result;

  Arg = goal_desc->worker_registers;
  Arg->next_insn = startgoalcode;
  Arg->node->term[0] = X(0);

#if defined(DEBUG) && defined(THREADS)
  if (debug_threads) printf("Goal %x entering wam()\n", (int)goal_desc);
#endif
  if ((wam_result = wam(Arg, goal_desc)) == WAM_ABORT) 
    MAJOR_FAULT("Wam aborted!")
  
#if defined(DEBUG) && defined(THREADS)
  if (debug_threads) printf("Goal %x exited wam()\n", (int)goal_desc);
#endif

  flush_output(Arg);

  /* eng_wait() may change NEEDS_FREEING and consults the state of the
     thread; therefore we lock until it is settled down */

  Wait_Acquire_slock(goal_desc->goal_lock_l);
  if (goal_desc->worker_registers->next_alt == termcode){
    unlink_wam(goal_desc);	/* We can make the WAM available right now */
    goal_desc->state = FAILED;
  } else goal_desc->state = PENDING_SOLS;

/* In some cases (i.e., Win32) the resources used up by the thread are
   not automatically freed upon thread termination.  If needed, the
   thread handle is enqued. In an (I hope) future implementation the
   thread will go to sleep instead of dying. */

  if (goal_desc->action & NEEDS_FREEING){ /* Implies thread created */
#if defined(DEBUG) && defined(THREADS)
    if (debug_threads) printf("Goal %x enqueuing itself\n", (int)goal_desc);
#endif
    enqueue_thread(goal_desc->thread_handle); /* Free, enqueue myself */
  } else   
    enqueue_thread((THREAD_T)NULL); /* Free whoever was there, enqueue no one*/
  
/* Save the state for the exit result (if we release the goal, its
   state may change before we return from the function). */
  result_state = goal_desc->state;

/* Goals failed when executed by the local thread, and goals for which
   no Id was requested, release the memory areas automatically */
  if ((wam_result == WAM_INTERRUPTED) ||
      !(goal_desc->action & KEEP_STACKS) ||
      ((goal_desc->state == FAILED) && !(goal_desc->action & CREATE_THREAD)))
    make_goal_desc_free(goal_desc);

  Release_slock(goal_desc->goal_lock_l);

#if defined(DEBUG) && defined(THREADS)
  if (debug_threads || debug_conc)  printf("*** %d(%d) Goal %x is EXITING\n", 
           (int)Thread_Id, (int)GET_INC_COUNTER, (int)goal_desc);
#endif
  return (THREAD_RES_T)(result_state == PENDING_SOLS);
}


/* If we hit the initial ghost choicepoint, then it means that no
   solution was returned by this call.  If we call the
   make_backtracking() pirmitive, then KEEP_STACKS is true. */

THREAD_RES_T make_backtracking(THREAD_ARG wo)
{
  goal_descriptor_p goal_desc = (goal_descriptor_p)wo;
  int result_state;
  int wam_result;
  Argdecl = goal_desc->worker_registers;

  if ((wam_result = wam(Arg, goal_desc)) == WAM_ABORT)
    MAJOR_FAULT("Wam aborted while doing backtracking")

  flush_output(Arg);

  Wait_Acquire_slock(goal_desc->goal_lock_l);
  if (Arg->next_alt == termcode) {
    unlink_wam(goal_desc);
    goal_desc->state = FAILED;
  } else goal_desc->state = PENDING_SOLS;

  if ((goal_desc->action & NEEDS_FREEING) ||
      (wam_result == WAM_INTERRUPTED)) /* Implies thread created */
    enqueue_thread(goal_desc->thread_handle); /* Free, enqueue myself */
  else   
    enqueue_thread((THREAD_T)NULL); /* Free whoever was there, enqueue no one*/

  result_state = goal_desc->state;

  /*
  if ((goal_desc->state == FAILED) && !(goal_desc->action & CREATE_THREAD))
    make_goal_desc_free(goal_desc);
  */

  Release_slock(goal_desc->goal_lock_l);

  return (THREAD_RES_T)(result_state == PENDING_SOLS);
}






