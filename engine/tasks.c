/* Copyright (C) 1996,1997,1998, UPM-CLIP */

#include <stdio.h>

#if !defined(THREADS) || defined(SunOS4)
#include <signal.h>
#endif

#include "configure.h"
#include "datadefs.h"
#include "support.h"
#include "threads.h"
#include "task_areas.h"

/* Declarations for global functions accessed from here */

#include "tasks_defs.h"
#include "main_defs.h"
#include "initial_defs.h"


/* Local declarations */

/* If we are not using threads, this simply points to a single WRB state;
   i.e., the list is actually a singleton. */
wrb_state_p wrb_state_list;  

LOCK_ST wrb_state_list_st;
LOCK    wrb_state_list_l;
                       

static wrb_state_p create_wrb_state(Argdecl);
static wrb_state_p get_my_task_block(void);

void print_task_status(Arg)
     Argdecl;
{
  FILE *u_o = Output_Stream_Ptr->streamfile;

  wrb_state_p current_task = wrb_state_list;

  while(current_task != NULL){
    switch(current_task->state) {
    case IDLE:
      fprintf(u_o, "Inactive @ %lx\n", (long int)current_task);
      break;
    case WORKING:
      fprintf(u_o, "Active @ %lx", (long int)current_task);
      fprintf(u_o, "\tWam @ %lx", (long int)current_task->worker_registers);
      fprintf(u_o, "\tGoal Id %ld", (long int)current_task->goal_id);
      fprintf(u_o, "\tThread Id %ld\n", (long int)current_task->thread_id);
      break;
    case WAITING:
      fprintf(u_o, "Waiting @ %lx", (long int)current_task);
      fprintf(u_o, "\tWam @ %lx", (long int)current_task->worker_registers);
      fprintf(u_o, "\tGoal Id %ld\n", (long int)current_task->goal_id);
      break;
    default:
      fprintf(u_o, "Unknown status (%ld) @ %lx\n", 
              (long int)current_task->state, (long int)current_task);
    }
    current_task = current_task->next;
  }
}


 /* Try to kill a thread (and release the wam it was attached to).  Return 0
    if no error, -1 on error (maybe no such thread). */

int kill_thread(this_worker)
     wrb_state_p this_worker;
{
  return Thread_Cancel(this_worker->thread_id);
}


#if defined(UNDEFINED)
/* Kill all threads but the current one; release the WAMs */

void kill_other_threads()
{
  wrb_state_p this_task;
  BOOL thread_killed;
  THREAD_T myself = Thread_Id;

  do {
    thread_killed = FALSE;
    Wait_Acquire_lock(wrb_state_list_l);
    this_task = wrb_state_list;
    
    while (this_task != NULL) {
      if (!Thread_Equal(this_task->thread_id, myself) &&
          (this_task->state == WORKING)) {
        kill_thread(this_task);
        this_task->state = IDLE;
        thread_killed = TRUE;
      } 
      this_task = this_task->next;
    }
    
    Release_lock(wrb_state_list_l);
  } while(thread_killed);
}
#endif




/* Cause kills to this thread to be immediately executed */

void allow_thread_cancellation()
{
  Allow_Thread_Cancel;
}


/* Cause kills to this thread to be ignored (for symmetry with the above) */

void disallow_thread_cancellation()
{
  Disallow_Thread_Cancel;
}



void init_wrb_state_list()
{
  wrb_state_list = (wrb_state_p)NULL;
  wrb_state_list_l = &wrb_state_list_st;
  Init_lock(wrb_state_list_l);
}


static wrb_state_p create_wrb_state(Arg)
     Argdecl;
{
  wrb_state_p new_wrb_state;

  new_wrb_state = (wrb_state_p)checkalloc(sizeof(wrb_state));
  new_wrb_state->worker_registers = Arg;
  return new_wrb_state;
}


/* Insert a wrb into the wrb state list and signal this wrb is ours. I am
  using a single linked linear list */

wrb_state_p attach_me_to_wrb_state_list(Arg)
     Argdecl;
{
  wrb_state_p new_wrb_state;

  new_wrb_state = create_wrb_state(Arg);/* create, assign worker registers */
  Wait_Acquire_lock(wrb_state_list_l);
  new_wrb_state->next = wrb_state_list;
  wrb_state_list = new_wrb_state;
  new_wrb_state->state = WORKING;
  Release_lock(wrb_state_list_l);
  return new_wrb_state;
}


/* A wrb state is to be marked as free --- no thread is working on it.  It
   is not, however, deleted from the state list, for creating areas is a
   costly process.  Should have exclusive access.  The WAM is not
   reinitialised: it is done upon acquiring a new goal. */

void make_worker_free(worker)
     wrb_state_p worker;
{
  worker->state = IDLE;
}

/* Return wrb state for the wrb ID, or return NULL if not found */

static wrb_state_p get_my_task_block()
{
  THREAD_T thr_id = Thread_Id;
  wrb_state_p this_task = wrb_state_list;

 /* Do not modify them while looking for work */

  Wait_Acquire_lock(wrb_state_list_l);

  while( (this_task != NULL) &&
         ((this_task->state == IDLE ) ||
          !Thread_Equal(this_task->thread_id, thr_id)))
    this_task = this_task->next;

  Release_lock(wrb_state_list_l);

  return this_task;
}

struct worker *get_my_worker()
{
  wrb_state_p state;

  if ((state = get_my_task_block()) == NULL)
    SERIOUS_FAULT("Could not find task block in get_my_worker()");

  return state->worker_registers;
}


/* Search for a free worker in the ring of workers.  Mark it as WORKING as
   soon as we find one free so that no other thread can steal it. */

wrb_state_p look_for_a_free_worker()
{

  wrb_state_p this_task = wrb_state_list;

#if defined(THREADS)
  Wait_Acquire_lock(wrb_state_list_l);
#endif
  while ((this_task != NULL) && (this_task->state != IDLE))
    this_task = this_task->next;  

  if (this_task != NULL) {
    this_task->state = WORKING;
#if defined(THREADS)
    Release_lock(wrb_state_list_l);
#endif
    local_init_each_time(this_task->worker_registers);
  } else
#if defined(THREADS)
    Release_lock(wrb_state_list_l)
#endif
    ;
  
  return this_task;
}

