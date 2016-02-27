/* Copyright (C) 2007,2008 UPM-CLIP */

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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "apll.h"
#include "visandor.h"


node_t *start_choice = NULL;

#if defined(ANDPARALLEL)
/* local atoms */
tagged_t fifo;
tagged_t lifo;
tagged_t det;

#endif

/* Checks whether a particular variable is a callable structure or not */
#define NOT_CALLABLE(What) IsVar(What) || TagIsSmall(What) || TagIsLarge(What)
#define ENSURE_CALLABLE(What, ArgNum)   \
if (NOT_CALLABLE(What))  {printf("\nNot Callable\n"); }
//  BUILTIN_ERROR(TYPE_ERROR(CALLABLE), What, ArgNum)


/*****************************************************************/
/*            ANDPROLOG LOW-LEVEL SUPPORT PRIMITIVES             */
/*****************************************************************/


/* Starts a new thread in the system */
bool_t apll_start_thread(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  goal_descriptor_t *gd = NULL;

  // Make sure we are calling a callable term!
  DEREF(X(0),X(0));
  ENSURE_CALLABLE(X(0),1);

  gd = gimme_a_new_gd();

  gd->goal = X(0);
  gd->action = CREATE_WAM | KEEP_STACKS | CREATE_THREAD;
  {  // Copy goal to remote thread
    Argdecl = gd->worker_registers;
    DEREF(X(0), cross_copy_term(Arg, gd->goal));
  }
  gd->action |= NEEDS_FREEING;
  Thread_Create_GoalId(startgoal, gd, gd->thread_id, gd->thread_handle);

  // Increase the number of agents in the system
  Wait_Acquire_slock(nagents_l);
  nagents++;
  Release_slock(nagents_l);
#endif

  return TRUE;
}


/* Returns the number of agents available in the system */
bool_t apll_number_agents(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  Wait_Acquire_slock(nagents_l);
  cunify(Arg, X(0), MakeInteger(w, (int)nagents));
  Release_slock(nagents_l);
#endif

  return TRUE;
}


#if defined(ANDPARALLEL)
/* Reads a handler */
par_handler_t *read_handler(hdler)
     tagged_t hdler;
{
  CIAO_REGISTER tagged_t x1 = (tagged_t)NULL;

  // Obtain the handler struct
  DEREF(hdler,hdler);
  DerefSwitch(hdler,x1,;);
  if (TagIsSTR(hdler) && (TagToHeadfunctor(hdler) == functor_Dhandler)) {
    DerefArg(x1,hdler,1);
    return (par_handler_t *)TermToPointer(x1);
  }
  else
    return NULL;
}


/* Adds handler to the goal list */
void init_handler(Arg, h, goal, determ, str)
     Argdecl;
     par_handler_t *(*h);
     tagged_t goal;
     tagged_t determ;
     tagged_t str;
{
  DEREF(goal,goal);
  DEREF(determ,determ);
  DEREF(str,str);

  if (!IsVar(goal))
    (*h)->goal = goal;
  if (!IsVar(determ))
    (*h)->det = (determ==det)?TRUE:FALSE;
  (*h)->exec_state = NOTEXECUTED;
  (*h)->agent = w;
  (*h)->remote_agent = NULL;
  (*h)->exec_limits = NULL;
  (*h)->gle = (handler_entry_t *)checkalloc(sizeof(handler_entry_t));
  ((*h)->gle)->handler = str;

#if defined(VISANDOR)
  // VisAndOr initialization
  (Pcall_Level(w))++;
  (*h)->ppf = Pcall_Level(w);
  EVENT(FORK, Pcall_Level(w), 2);
#endif
}


/* Adds handler to the goal list */
void add_handler_to_goal_list(Arg, gle)
     Argdecl;
     handler_entry_t *(*gle);
{
  if (Goal_List_Top != NULL) {
    (*gle)->prev = Goal_List_Top;
    Goal_List_Top->next = (*gle);
    Goal_List_Start->prev = (*gle);
    Goal_List_Top = (*gle);
  }
  else {
    Goal_List_Start = (*gle);
    Goal_List_Top = (*gle);
    (*gle)->prev = Goal_List_Top;
  }
  (*gle)->next = Goal_List_Start;
}


/* Removes handler from the goal list */
void remove_handler_from_goal_list(Arg, gle, free)
     Argdecl;
     handler_entry_t *(*gle);
     bool_t free;
{
  if ((*gle) != NULL) {
    if (Goal_List_Start == Goal_List_Top) {
      Goal_List_Start = NULL;
      Goal_List_Top = NULL;
    }
    else {
      ((*gle)->next)->prev = (*gle)->prev;
      ((*gle)->prev)->next = (*gle)->next;
      if (*gle == Goal_List_Top)
	Goal_List_Top = (*gle)->prev;
      if (*gle == Goal_List_Start)
	Goal_List_Start = (*gle)->next;
    }
    if (free) {
      checkdealloc((tagged_t *)(*gle),sizeof(handler_entry_t));
      (*gle) = NULL;
    }
  }
}


void send_event(w,tagged_h,canc)
     Argdecl;
     tagged_t tagged_h;
     bool_t canc;
{
  par_handler_t *h = read_handler(tagged_h);

  if (h != NULL) {
    worker_t *remote_a = (worker_t *)(h->remote_agent);
    if ((remote_a != NULL) && (remote_a != w)) {
#if defined(DEBUG)
      if (debug_threads) {
	printf("send_event("); prolog_display(Arg);
	printf(",%d) from %x to %x\n",canc,w,remote_a);
      }
#endif

      // Create a new entry for the event queue
      DEREF(X(0),X(0));
      event_entry_t *new_eqe =
	(event_entry_t *)checkalloc(sizeof(event_entry_t));
      new_eqe->handler = (tagged_t)read_handler(X(0));
      new_eqe->canc = canc;

      // Add entry to the remote event queue
      Wait_Acquire_slock(Event_Queue_Lock_Of(remote_a));
      if (Event_Queue_Top_Of(remote_a) != NULL) {
	new_eqe->prev = Event_Queue_Top_Of(remote_a);
	Event_Queue_Top_Of(remote_a)->next = new_eqe;
	Event_Queue_Start_Of(remote_a)->prev = new_eqe;
	Event_Queue_Top_Of(remote_a) = new_eqe;
      }
      else {
	Event_Queue_Start_Of(remote_a) = new_eqe;
	Event_Queue_Top_Of(remote_a) = new_eqe;
	new_eqe->prev = Event_Queue_Top_Of(remote_a);
      }
      new_eqe->next = Event_Queue_Start_Of(remote_a);
      Release_slock(Event_Queue_Lock_Of(remote_a));
    }
  }
}
#endif


/* Pushes the handler associated to a goal on to the goal list */
bool_t apll_push_goal(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  push_choicept(Arg,address_nd_environment_protection_c);
  par_handler_t *h = NULL;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  if (!IsVar(X(0))) ENSURE_CALLABLE(X(0),1);

  // Create or read the handler
  if (IsVar(X(2))) {  // Create
    // Allocating memory for the handler
    h = (par_handler_t *)checkalloc(sizeof(par_handler_t));

    // Create the structure in the heap
    CIAO_REGISTER tagged_t *pt1 = w->global_top;
    HeapPush(pt1,functor_Dhandler);
    HeapPush(pt1,PointerToTerm((int)h));
    w->global_top = pt1;
    CTagToPointer(X(2)) = Tag(STR, HeapOffset(pt1,-2));

    // Initialize handler and add it to the goal list
    DEREF(X(2),X(2));
    Wait_Acquire_slock(Goal_List_Lock);
    init_handler(w,&h,X(0),X(1),X(2));
    add_handler_to_goal_list(w,&(h->gle));
    unwinding_done = FALSE;
  }
  else {  // Read
    // Initialize handler and add it to the goal list, if not in it yet
    h = read_handler(X(2));
    if (h != NULL) {
      Wait_Acquire_slock(Goal_List_Lock);
      init_handler(w,&h,X(0),X(1),X(2));
      add_handler_to_goal_list(w,&(h->gle));
    }
  }

  // Incrementing the number of parallel goals in the system
  Wait_Acquire_slock(npargoals_l);
  npargoals++;
  Release_slock(npargoals_l);

  Release_slock(Goal_List_Lock);

#if defined(DEBUG)
  if (debug_threads) {
    tagged_t aux_x0 = X(0); X(0) = X(2);
    printf("Goal pushed: "); prolog_display(Arg); printf(" by %x\n",w);
    X(0) = aux_x0;
  }
#endif
#endif

  return TRUE;
}


/* Finds a handler associated to a goal from the goal list of some agent */
bool_t apll_find_goal(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  bool_t found_goal = FALSE;
  worker_t *aux = Next_Wam_Of(w);
  handler_entry_t *gle = NULL;
  par_handler_t *h = NULL;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  if (aux != w) {
    do {
      Wait_Acquire_slock(Goal_List_Lock_Of(aux));
      if (Goal_List_Top_Of(aux) != NULL && Goal_List_Start_Of(aux) != NULL) {

	// Pick up the handler
	if (X(0) == fifo)  // FIFO scheduling
	  gle = Goal_List_Start_Of(aux);
	else if (X(0) == lifo)  // LIFO scheduling
	  gle = Goal_List_Top_Of(aux);
	else
	  MAJOR_FAULT("Goal scheduling error in apll_find_goal.");

	// Incrementing the number of parallel goals that have been stoled
	Wait_Acquire_slock(npargoalstaken_l);
	if (npargoalstaken != NULL && measure) (npargoalstaken->value)++;
	Release_slock(npargoalstaken_l);

	h = read_handler(gle->handler);
	if (h != NULL) {
	  if ((X(1) != det) || ((X(1) == det) && (h->det))) {
	    // Give value to remote_agent in the handler
	    h->remote_agent = w;

	    // Return a pointer to the remote goal in X(2)
	    cunify(Arg, X(2), gle->handler);

#if defined(DEBUG)
	    if (debug_threads) {
	      tagged_t aux_x0 = X(0); X(0) = X(2);
	      printf("Handler found: "); prolog_display(Arg);
	      printf(" by %x\n",w);
	      X(0) = aux_x0;
	    }
#endif

	    // Remove handler from goal list in remote agent
	    remove_handler_from_goal_list(aux,&(h->gle),TRUE);
	    h->exec_state = REM_EXECUTING;
	    h->gle = NULL;

	    // Exit
	    found_goal = TRUE;
	  }
	}
	else {
	  remove_handler_from_goal_list(aux,&gle,TRUE);
	  gle = NULL;
	}
      }
      Release_slock(Goal_List_Lock_Of(aux));
      aux = Next_Wam_Of(aux);
    }
    while ((!found_goal) && (aux != w));
  }

  return found_goal;
#else
  return TRUE;
#endif
}


/* Succeeds if the handler has not been picked up yet by another agent
   and fails otherwise */
bool_t apll_goal_available(Arg)
     Argdecl;
{
  bool_t result = TRUE;
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  // Check whether the goal has been taken by another agent or not
  if (h != NULL) {
    //    Wait_Acquire_slock(Goal_List_Lock);
    if ((h->gle == NULL) && (h->remote_agent != w))
      result = FALSE;
    else {
      h->remote_agent = w;
      remove_handler_from_goal_list(w,&(h->gle),TRUE);
      h->gle = NULL;
    }
    //    Release_slock(Goal_List_Lock);
  }
  else
    result = FALSE;
#endif

  return result;
}


///* Frees the memory used by the handler */
bool_t apll_cancellation(Arg)
     Argdecl;
{
//#if defined(ANDPARALLEL)
//  par_handler_t *h = read_handler(X(0));
//
//  if (h != NULL) {
//    Wait_Acquire_slock(Goal_List_Lock);
//    if (h->gle != NULL) {  // Handler still in goal list
//      remove_handler_from_goal_list(w,&(h->gle),TRUE);
//      h->exec_state = CANCELLED;
//      h->gle = NULL;
//      Release_slock(Goal_List_Lock);
//    }
//    else {
//      if (!h->det) {
//	if ((h->remote_agent != NULL) && (h->remote_agent != w) &&
//            (h->agent == w)) {
//	  Release_slock(Goal_List_Lock);
//	  Wait_Acquire_slock(Mutex_Lock);
//	  worker_t *rem_a = h->remote_agent;
//	  if (h->exec_state == NOTEXECUTED) {  // done
//	    h->exec_state = CANCELLED;	    
//	    Release_slock(Mutex_Lock);
//	  }
//	  else if (h->exec_state == REM_EXECUTING) {  // done
//	    if (Cancel_Goal_Exec_Handler_Of(h->remote_agent) != NULL) printf("\nBUG CANCEL I\n");
//	    Cancel_Goal_Exec_Handler_Of(h->remote_agent) = h;
//	    h->exec_state = CANCELLING;
//	    Release_slock(Mutex_Lock);
//	  }
//	  else if (h->exec_state == FINISHED) {  // done
//	    h->exec_state = CANCELLING;
//	    Release_slock(Mutex_Lock);
//	    Wait_Acquire_slock(Mutex_Lock_Of(rem_a));
//	    send_event(w,X(0),TRUE);
//	    if (Suspended_Waiting_For_Work_Of(rem_a)) {
//	      Wait_Acquire_lock(Waiting_For_Work_Lock_Of(rem_a));
//	      Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(rem_a));
//	      Release_lock(Waiting_For_Work_Lock_Of(rem_a));
//	    }
//	    Release_slock(Mutex_Lock_Of(rem_a));
//	  }
//	  else if (h->exec_state == NO_MORE_SOLS) {  // done
//	    h->exec_state = CANCELLED;	    
//	    Release_slock(Mutex_Lock);
//	  }
//	  else if (h->exec_state == MORE_SOLS) {  // done
//	    if (Cancel_Goal_Exec_Handler_Of(h->remote_agent) != NULL) printf("\nBUG CANCEL III\n");
//	    //Cancel_Goal_Exec_Handler_Of(h->remote_agent) = h;
//	    h->exec_state = CANCELLING;
//	    Release_slock(Mutex_Lock);
//	  }
//	  else if (h->exec_state == REEXECUTE) {  // done
//	    h->exec_state = CANCELLED;
//	    Release_slock(Mutex_Lock);
//
//	    // Find and remove entry from the event queue
//	    Wait_Acquire_slock(Event_Queue_Lock_Of(h->remote_agent));
//	    bool_t found = FALSE;
//	    event_entry_t *aux = Event_Queue_Start_Of(h->remote_agent);
//	    while ((aux != Event_Queue_Top_Of(h->remote_agent)) && !found) {
//	      if (aux->handler == h)
//		found = TRUE;
//	      aux = aux->next;
//	    }
//	    if (found) {
//	      if (Event_Queue_Start_Of(h->remote_agent) == Event_Queue_Top_Of(h->remote_agent)) {
//		Event_Queue_Start_Of(h->remote_agent) = NULL;
//		Event_Queue_Top_Of(h->remote_agent) = NULL;
//	      }
//	      else if (Event_Queue_Start_Of(h->remote_agent) == aux) {
//		aux->next->prev = aux->prev;
//		aux->prev->next = aux->next;
//		Event_Queue_Start_Of(h->remote_agent) = aux->next;
//	      }
//	      else if (Event_Queue_Top_Of(h->remote_agent) == aux) {
//		aux->next->prev = aux->prev;
//		aux->prev->next = aux->next;
//		Event_Queue_Top_Of(h->remote_agent) = aux->prev;
//	      }
//	      else {
//		aux->next->prev = aux->prev;
//		aux->prev->next = aux->next;
//	      }
//	    }
//	    checkdealloc((tagged_t *)aux, sizeof(event_entry_t));
//	    Release_slock(Event_Queue_Lock_Of(h->remote_agent));
//	  }
//	  else { Release_slock(Goal_List_Lock); printf("ELSE\n"); }
//	}
//	else
//	  {
//	    h->exec_state = CANCELLED;
//	    Release_slock(Goal_List_Lock);
//	  }
//      }
//      else {
//	Release_slock(Goal_List_Lock);
//      }
//    }
//
//    // Free memory used for handler
//    //checkdealloc((tagged_t *)h,sizeof(par_handler_t));
//
//#if defined(DEBUG)
//    if (debug_threads) {
//      printf("Cancellation done: "); prolog_display(Arg); printf(" by %x\n",w);
//    }
//#endif
//  }
//#endif
//
  return TRUE;
}


/* Returns the goal associated to the handler */
bool_t apll_retrieve_goal(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    return cunify(Arg, X(1), h->goal);
#endif

  return TRUE;
}


/* Returns whether the goal is deterministic or not */
bool_t apll_goal_det(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    return h->det;
  else
    return FALSE;
#endif

  return TRUE;
}


/* Sets the goal to deterministic */
bool_t apll_set_goal_det(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    if (h->agent == w || h->remote_agent == w)
      h->det = TRUE;
#endif

  return TRUE;
}


/* Sets the goal to non-deterministic */
bool_t apll_set_goal_nondet(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    if (h->agent == w || h->remote_agent == w)
      h->det = FALSE;
#endif

  return TRUE;
}


/* Returns whether the goal has not been executed yet */
bool_t apll_goal_not_executed(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    return (h->exec_state == NOTEXECUTED);
  else
    return FALSE;
#endif

  return TRUE;
}


/* Sets the goal execution to never executed */
bool_t apll_set_goal_not_executed(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL) {
    if (h->agent == w || h->remote_agent == w)
      h->exec_state = NOTEXECUTED;
    else
      return FALSE;
  }
  else
    return FALSE;
#endif

  return TRUE;
}


/* Returns whether the goal is remotely executing */
bool_t apll_goal_rem_executing(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    return (h->exec_state == REM_EXECUTING);
  else
    return FALSE;
#endif

  return TRUE;
}


/* Sets the goal execution to being remotely executing */
bool_t apll_set_goal_rem_executing(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    if (h->agent == w || h->remote_agent == w)
      h->exec_state = REM_EXECUTING;
#endif

  return TRUE;
}


/* Returns whether the goal execution has finished or not */
bool_t apll_goal_finished(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    return (h->exec_state == FINISHED);
  else
    return FALSE;
#endif

  return TRUE;
}


/* Sets the goal execution to finished */
bool_t apll_set_goal_finished(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    if (h->agent == w || h->remote_agent == w)
      h->exec_state = FINISHED;
#endif

  return TRUE;
}


/* Returns whether the goal execution has to backtrack or not */
bool_t apll_goal_tobacktrack(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    return (h->exec_state == MORE_SOLS);
  else
    return FALSE;
#endif

  return TRUE;
}

/* Sets the goal execution to be backtracked */
bool_t apll_set_goal_tobacktrack(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    if (h->agent == w || h->remote_agent == w)
      h->exec_state = MORE_SOLS;
#endif

  return TRUE;
}


/* Returns whether the goal execution has to be reexecuted or not */
bool_t apll_goal_toreexecute(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    {
      return (h->exec_state == REEXECUTE);
    }
  else
    return FALSE;
#endif

  return TRUE;
}


/* Sets the goal execution to be reexecuted */
bool_t apll_set_goal_toreexecute(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    if (h->agent == w || h->remote_agent == w)
      h->exec_state = REEXECUTE;
#endif

  return TRUE;
}


/* Returns whether the goal execution has failed or not */
bool_t apll_goal_failed(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    return (h->exec_state == NO_MORE_SOLS);
  else
    return FALSE;
#endif

  return TRUE;
}


/* Sets the goal execution to finished */
bool_t apll_set_goal_failed(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    if (h->agent == w || h->remote_agent == w)
      h->exec_state = NO_MORE_SOLS;
#endif

  return TRUE;
}


/* Returns whether the goal execution has been cancelled or not */
bool_t apll_goal_cancelled(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    return (h->exec_state == CANCELLED);
  else
    return FALSE;
#endif

  return TRUE;
}

/* Returns whether the goal execution has been cancelled or not */
bool_t apll_show_handler(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));
#endif

  return TRUE;
}


/* Sets the goal execution to cancelled */
bool_t apll_set_goal_cancelled(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    if (h->agent == w || h->remote_agent == w)
      h->exec_state = CANCELLED;
#endif

  return TRUE;
}


/* Sends the goal to the agent that picked it up in order to perform
   backtracking over it */
bool_t apll_send_event(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  send_event(w,X(0),FALSE);
#endif

  return TRUE;
}


/* Reads an event from the event queue and fails if it is empty */
bool_t apll_read_event(Arg)
     Argdecl;
{
  bool_t ret = TRUE;

#if defined(ANDPARALLEL)
  Wait_Acquire_slock(Event_Queue_Lock);
  if ((Event_Queue_Start == NULL) && (Event_Queue_Top == NULL)) {
    ret = FALSE;
  }
  else {

    // Create the structure in the heap
    CIAO_REGISTER tagged_t *pt1 = w->global_top;
    HeapPush(pt1,functor_Dhandler);
    HeapPush(pt1,PointerToTerm((int)Event_Queue_Start->handler));
    w->global_top = pt1;
    CTagToPointer(X(0)) = Tag(STR, HeapOffset(pt1,-2));

#if defined(DEBUG)
    if (debug_threads) {
      printf("read_event("); prolog_display(Arg); printf(") by %x\n",w);
    }
#endif

    // Remove entry from the event queue
    event_entry_t *eqe = NULL;
    if (Event_Queue_Start == Event_Queue_Top) {
      eqe = Event_Queue_Start;
      Event_Queue_Start = NULL;
      Event_Queue_Top = NULL;
    }
    else {
      (Event_Queue_Start->next)->prev = Event_Queue_Start->prev;
      (Event_Queue_Start->prev)->next = Event_Queue_Start->next;
      eqe = Event_Queue_Start;
      Event_Queue_Start = Event_Queue_Start->next;
    }
    //checkdealloc((tagged_t *)eqe, sizeof(event_entry_t));
  }
  Release_slock(Event_Queue_Lock);
#endif

  return ret;
}


/* Saves a pointer to the initial choice point of the goal execution
   in the handler */
bool_t apll_save_init_execution(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (start_choice == NULL) start_choice = w->node;

  if (h != NULL) {
    if (h->exec_limits == NULL) {
      (h->exec_limits) = (parallel_exec_entry_t *)
                         checkalloc(sizeof(parallel_exec_entry_t));
      (h->exec_limits)->init = w->node;
      (h->exec_limits)->end = w->node;
    }
    else {
      (h->exec_limits)->init = w->node;
    }
  }
#endif

  return TRUE;
}


/* Saves a pointer to the final choice point of the goal execution in
   the handler */
bool_t apll_save_end_execution(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL) {
    if (h->exec_limits != NULL) {
      (h->exec_limits)->end = w->node;
    }
//    ComputeA(w->local_top,w->node);
//    printf("\nHandler %p limits are %p and %p -> %p %p\n",h,(h->exec_limits)->init,(h->exec_limits)->end,
//	   w->local_top,(h->exec_limits)->end->local_top);
  }
#endif

  return TRUE;
}


/* Returns whether there are more solutions to be computed or not */
bool_t apll_more_solutions(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL) {
    if (h->exec_limits != NULL) {
      if (h->det)
	return (h->exec_limits)->init == (h->exec_limits)->end;
      else
	return !((h->exec_limits)->init == 
		 ChoiceCharOffset((h->exec_limits)->end,
		   -((h->exec_limits)->end)->next_alt->node_offset));
    }
  }
#endif

  return TRUE;
}

/* Moves the choice point of the parallel goal to the top of the
   stack */
bool_t apll_move_execution_top(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));
  
  node_t *node = w->node;

  // Move choice points of current parallel goal execution to top of stack
//  printf("\nMoving exec top %p\n",w->node);
//  printf("\ntrail node %p y trail %p\n",w->node->trail_top,w->trail_top);
  ComputeA(w->local_top,w->node);
  w->node = ChoiceCharOffset(w->node,-w->node->next_alt->node_offset);
//  printf("\nMoving exec top %p\n",w->node);
//  printf("\nMoving exec top %p\n",w->node); fflush(stdout);
//  printf("\ntrail node %p y trail %p\n",w->node->trail_top,w->trail_top);
  if (h != NULL) {
    if (h->exec_limits != NULL) {
      CIAO_REGISTER node_t *init = (h->exec_limits)->init;
      frame_t *init_frame = (h->exec_limits)->init->local_top;
      tagged_t *init_heap = (h->exec_limits)->init->global_top;
      CIAO_REGISTER node_t *end = (h->exec_limits)->end;
      if (ChoiceYounger(w->node, (h->exec_limits)->end)) {

//	printf("\nMoving trapped goal node %p - ini %p - end %p\n",w->node,init,end); fflush(stdout);
//	printf("\nHas to analyze agaist %p\n",h->exec_limits->end->local_top);

//	for(node = w->node; !ChoiceYounger(start_choice,node); node = ChoiceCharOffset(node, -node->next_alt->node_offset))
//	  {
//	    printf("\nNode %p=%p trail value %p=%p\n",node,node->next_alt,node->trail_top,CTagToPointer(node->trail_top));
//	    printf("\nProtects until %p %p\n",node->global_top,node->local_top);
//	  }

	tagged_t *itrail;
//	for(itrail = w->trail_top; !TrailYounger(start_choice->trail_top,itrail); itrail--)
//	  {
//	    printf("\nitrail %p=%p\n",itrail,CTagToPointer(itrail));
//	  }

	CIAO_REGISTER node_t *node, *node2, *pargoal_node, *younger_nodes;
	CIAO_REGISTER tagged_t *pargoal_trail, *younger_trail;

	// STEP ZERO: Calculate sizes of auxiliary data structures.
	// Stack and trail grow in different directions
	int size_pargoal_node =
          ChoiceCharDifference(ChoiceCharOffset(init, -init->next_alt->node_offset), end);
	int size_younger_nodes =
	  ChoiceCharDifference(end,w->node);

	for(node = w->node; node != end; node = ChoiceCharOffset(node, -node->next_alt->node_offset))
	  node2 = node;

	int size_pargoal_trail =
          TrailCharDifference(init->trail_top, node2->trail_top);
	int size_younger_trail =
	  TrailCharDifference(node2->trail_top, w->trail_top);

	// STEP ONE: save memory for auxiliary data structures
	// Behavior of memcpy when overlapping is undetermined.
	// Therefore, we need to move younger section of stack to another
        // auxiliary place in memory before moving it down on the stack
        pargoal_node = (node_t *) checkalloc(size_pargoal_node);
        younger_nodes = (node_t *) checkalloc(size_younger_nodes);
	pargoal_trail = (tagged_t *) checkalloc(size_pargoal_trail);
	younger_trail = (tagged_t *) checkalloc(size_younger_trail);

        // STEP TWO: Copy choice points of goal execution and trail to
        // auxiliary data structures
       	memcpy(pargoal_node, end, size_pargoal_node);
       	memcpy(younger_nodes, w->node, size_younger_nodes);
	memcpy(pargoal_trail, init->trail_top, size_pargoal_trail);
	memcpy(younger_trail, node2->trail_top, size_younger_trail);


	// STEP THREE: Move younger choice points and trail section down
	w->node = ChoiceCharOffset(w->node, -size_pargoal_node);
	memcpy(init->trail_top, younger_trail, size_younger_trail);
	memcpy(w->node, younger_nodes, size_younger_nodes);

	// STEP FOUR: Update trail pointers of choice points that are
	// moved down the stack
	node = node2 = w->node;
	while (!ChoiceYounger(init, node2))
	  {
	    node2->trail_top = TrailCharOffset(node2->trail_top, -size_pargoal_trail);
	    node2 = ChoiceCharOffset(node2,-node2->next_alt->node_offset);
	  }
	
	// STEP FIVE: Move pargoal_node to top of stack and
	// pargoal_trail to top of trail
      	w->node = ChoiceCharOffset(w->node, size_pargoal_node);
	memcpy(w->node, pargoal_node, size_pargoal_node);
	memcpy(TrailCharOffset(w->trail_top, -size_pargoal_trail), pargoal_trail, size_pargoal_trail);

	// STEP SIX: Update top of heap, local stack of oldest choice point
        init->local_top = init_frame;
	init->global_top = init_heap;

/* 	ComputeA(w->local_top,w->node); */
/* 	NewShadowregs(w->global_top); */
/* 	SaveGtop(w->node, w->global_top); */
/* 	SaveLtop(w->node); */
/* 	SetShadowregs(w->node); */

	// STEP NINE: Update trail pointers of choice points that are
	// moved on top of the stack
	node2 = w->node;

	while (ChoiceYounger(node2, node)) {
	  node2->global_top = w->global_top;
	  node2->local_top = w->local_top;
	  node2->trail_top = TrailCharOffset(node2->trail_top, size_younger_trail);
	  node2 = ChoiceCharOffset(node2,-node2->next_alt->node_offset);
	}

	//NEED TO BE REVISED!!!
	node2 = w->node;
	while (ChoiceYounger(node2, Choice_Start)) {
	    int i;
	    for (i = 0; i < OffsetToArity(node2->next_alt->node_offset); i++)
	      {
		par_handler_t *ht = read_handler(node2->term[i]);
		if ((ht != NULL) && !(ht->goal & 1) && (ht->exec_limits != NULL))
		  {
		    ht->goal = ht->goal | 1;
		    //remote handler??
		    if ((ChoiceYounger(Choice_Start,ht->exec_limits->init)) || (ChoiceYounger(ht->exec_limits->end, Choice_End)))
		      {
			continue;
		      }
		    //is init in the affected choice segment?
		    if (!ChoiceYounger(init,ht->exec_limits->init))
		      {
			//is init in the younger choice segment?
			if (ChoiceYounger(ht->exec_limits->init,ChoiceCharOffset(w->node,-size_younger_nodes)))
			  ht->exec_limits->init = ChoiceCharOffset(ht->exec_limits->init,-size_pargoal_node);
			else 
			  ht->exec_limits->init = ChoiceCharOffset(ht->exec_limits->init,size_younger_nodes);			
		      }
		    //is end in the affected choice segment?
		    if (!ChoiceYounger(init,ht->exec_limits->end))
		      {
			//is end in the younger choice segment?
			if (ChoiceYounger(ht->exec_limits->end,ChoiceCharOffset(w->node,-size_younger_nodes)))
			  {
			    ht->exec_limits->end = ChoiceCharOffset(ht->exec_limits->end,-size_pargoal_node);
			  }
			else
			  {
			    ht->exec_limits->end = ChoiceCharOffset(ht->exec_limits->end,size_younger_nodes);	
			  }
		      }
//		    printf("\nNew handler %p limits are %p y %p -> protect %p - %p\n",ht,(ht->exec_limits)->init,
//			   (ht->exec_limits)->end,(ht->exec_limits)->init->local_top,(ht->exec_limits)->end->local_top);
		  }
	      }
	    node2 = ChoiceCharOffset(node2,-node2->next_alt->node_offset);
	}

	node2 = w->node;
	while (ChoiceYounger(node2, Choice_Start)) {
	  int i;
	  for (i = 0; i < OffsetToArity(node2->next_alt->node_offset); i++)
	    {
	      par_handler_t *ht = read_handler(node2->term[i]);
	      if (ht != NULL) ht->goal = ht->goal & 0xfffffffe;
	    }
	  node2 = ChoiceCharOffset(node2,-node2->next_alt->node_offset);
	}

	//STEP ELEVEN: remove trail cells pointing out of my environment
	for(itrail = w->trail_top; !TrailYounger(h->exec_limits->init->trail_top,itrail); itrail--)
	  {
//	    if (TagIsHVA(CTagToPointer(itrail)))
//	      {
//		if ((TagToPointer(CTagToPointer(itrail)) >= h->exec_limits->end->global_top) ||
//		    (TagToPointer(CTagToPointer(itrail)) < h->exec_limits->init->global_top))
//		  {
//		    if (OnHeap(TagToPointer(CTagToPointer(itrail))))
//		      {
//			printf("\nTHIS IS THE CASE!!!\n");
//			CTagToPointer(itrail) = 0;
//		      }
//		  }
//	      }  
	    if (TagIsSVA(CTagToPointer(itrail)))
	      {
//		printf("\nAnalizing %p(%p): %p - %p\n",CTagToPointer(itrail),
//		       TagToPointer(CTagToPointer(itrail)),
//		       h->exec_limits->init->local_top,
//		       h->exec_limits->end->local_top);
		if (((TagToPointer(CTagToPointer(itrail)) >= (tagged_t*)h->exec_limits->end->local_top) ||
		     (TagToPointer(CTagToPointer(itrail)) < (tagged_t*)h->exec_limits->init->local_top)) &&
		    OnStack(TagToPointer(CTagToPointer(itrail))))
		  {
//			printf("\nTHIS IS THE CASE!!!\n");
		    CTagToPointer(itrail) = 0;
		  }
	      }

	  }

	// CLEANUP? Increment the number of remote trapped backwards executions
	Wait_Acquire_slock(nrembacktr_trapped_l);
	if (nrembacktr_trapped != NULL && measure)
	  (nrembacktr_trapped->value)++;
	Release_slock(nrembacktr_trapped_l);

	checkdealloc((tagged_t*)pargoal_node,size_pargoal_node);
	checkdealloc((tagged_t*)younger_nodes,size_younger_nodes);
	checkdealloc(pargoal_trail,size_pargoal_trail);
	checkdealloc(younger_trail,size_younger_trail);

	SetShadowregs(w->node);
//	for(node = w->node; !ChoiceYounger(start_choice,node); node = ChoiceCharOffset(node, -node->next_alt->node_offset))
//	  {
//	    printf("\nNode %p=%p trail value %p=%p\n",node,node->next_alt,node->trail_top,CTagToPointer(node->trail_top));
//	    printf("\nProtects until %p %p\n",node->global_top,node->local_top);
//	  }
//	for(itrail = w->trail_top; !TrailYounger(start_choice->trail_top,itrail); itrail--)
//	  {
//	    printf("\nitrail %p=%p\n",itrail,CTagToPointer(itrail));
//	  }

      }
      else 
	{
	  if (w->node != (h->exec_limits)->end) printf("\nBUG!!!!\n");
	  SetShadowregs(w->node);
	}

//      if (h->exec_state == CANCELLING)
//	{
//	  if (Cancel_Goal_Exec_Handler != NULL) printf("\nBUG CANCEL II\n");
//	  Wait_Acquire_slock(Mutex_Lock_Of(h->remote_agent));
//	  Cancel_Goal_Exec_Handler = h;
//	  Release_slock(Mutex_Lock_Of(h->remote_agent));
//	}
//      else
//	{
//Following lines needed if mutex is released before the execution of move_execution_top
//	  Wait_Acquire_slock(Mutex_Lock_Of(h->agent));
	  h->exec_state = REM_EXECUTING;
//	  Release_slock(Mutex_Lock_Of(h->agent));
//	}
      
    }
  }
  
#endif

//  printf("\nEND NODE %p and trail node %p and trail %p\n",w->node,w->node->trail_top,w->trail_top);
  return TRUE;
}


/* Returns whether the agent associated to the input handler is
   waiting for some work or not */
bool_t apll_waiting(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL)
    return Suspended_Waiting_For_Work_Of(h->agent);
#endif

  return TRUE;
}


/* Suspends the execution of the current thread */
bool_t apll_suspend(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
#if defined(VISANDOR)
  EVENT(AGENT_IDLE,0,nacagents);
#endif

//  printf("\nPRE waiting for work"); fflush(stdout);
  Wait_Acquire_lock(Waiting_For_Work_Lock);
//  printf("\nPOST waiting for work"); fflush(stdout);
#if defined(DEBUG)
  if (debug_threads)
    printf("Suspending: %x\n",w);
#endif
  if (Mode == UNWIND) Mode = FORWARD_EXEC;
  Suspended_Waiting_For_Work = TRUE;
  Release_slock(Mutex_Lock);
  Suspend = WAITING;

#if defined(Solaris)
  //Measuring the active time of the agent
  if (measure)
    Suspending_Time_Cont = gethrtime();
  if (Total_Suspending_Time != NULL && measure)
    Total_Suspending_Time->n++;
#endif

  Cond_Var_Wait(Waiting_For_Work_Cond_Var,Waiting_For_Work_Lock);

#if defined(Solaris)
  // Measuring the active time of the agent
  if (Total_Suspending_Time != NULL && measure)
    Total_Suspending_Time->time = Total_Suspending_Time->time +
      (double)(gethrtime() - Suspending_Time_Cont);
#endif
  
  Suspend = RELEASED;
#if defined(DEBUG)
  if (debug_threads)
    printf("Releasing: %x\n",w);
#endif
  Suspended_Waiting_For_Work = FALSE;
  Release_lock(Waiting_For_Work_Lock);
  if (Mode == FORWARD_EXEC)  Wait_Acquire_slock(Mutex_Lock);

#if defined(VISANDOR)
  EVENT(AGENT_BUSY,0,nacagents);
#endif

  return (Mode == FORWARD_EXEC);
#endif

  return TRUE;
}


/* Releases the execution of the publishing agent */
bool_t apll_release(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL) {
    if (h->remote_agent == w && h->exec_state >= 0 &&
        h->exec_state < NUMBER_STATES) {
      worker_t *ag = h->agent;
      if ((ag != NULL) && (ag != w) && (Suspended_Waiting_For_Work_Of(ag))) {
	Wait_Acquire_lock(Waiting_For_Work_Lock_Of(ag));
	Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(ag));
	Release_lock(Waiting_For_Work_Lock_Of(ag));
      }
    }
  }
#endif

  return TRUE;
}


/* Releases the execution agent that picked up the parallel goal */
bool_t apll_release_remote(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL) {
    if (h->agent == w) {
      worker_t *ag = h->remote_agent;
      if ((ag != NULL) && (ag != w) && (Suspended_Waiting_For_Work_Of(ag))) {
	Wait_Acquire_lock(Waiting_For_Work_Lock_Of(ag));
	Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(ag));
	Release_lock(Waiting_For_Work_Lock_Of(ag));
      }
    }
  }
#endif

  return TRUE;
}


/* Releases some suspended agent */
bool_t apll_release_some_suspended_thread(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  bool_t released_agent = FALSE;
  worker_t *aux = Next_Wam_Of(w);

  // Release a suspended agent, if there is one
  do {
    Wait_Acquire_slock(Mutex_Lock_Of(aux));
    if (Suspended_Waiting_For_Work_Of(aux)) {
      Wait_Acquire_lock(Waiting_For_Work_Lock_Of(aux));
      Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(aux));
      Release_lock(Waiting_For_Work_Lock_Of(aux));
      released_agent = TRUE;
    }
    Release_slock(Mutex_Lock_Of(aux));
    aux = Next_Wam_Of(aux);
  }
  while (!released_agent && (aux != w));
#endif

  return TRUE;
}


/* Releases the execution of the agents to perform stack unwinding */
bool_t apll_release_all_for_unwinding(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  if ((main_worker == w) && (!unwinding_done)) {
    worker_t *aux = w;
    for (aux=Next_Wam_Of(w); aux!=w; aux=Next_Wam_Of(aux))
      Mode_Of(aux) = UNWIND;
    for (aux=Next_Wam_Of(w); aux!=w; aux=Next_Wam_Of(aux)) {
      if (Suspended_Waiting_For_Work_Of(aux)) {
	Wait_Acquire_lock(Waiting_For_Work_Lock_Of(aux));
	Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(aux));
	Release_lock(Waiting_For_Work_Lock_Of(aux));
      }
    }
    unwinding_done = TRUE;
  }
#endif

  return TRUE;
}


/* Attemps to enter into the mutual exclusion */
bool_t apll_enter_mutex(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL && h->exec_state >= 0 && h->exec_state < NUMBER_STATES) {
    if ((h->agent != NULL) && (h->agent != w)) {
      Safe_To_Cancel = FALSE;
      Wait_Acquire_slock(Mutex_Lock_Of(h->agent));
    }
  }
#endif

  return TRUE;
}


/* Attemps to enter into the local mutual exclusion */
bool_t apll_enter_mutex_self(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  Safe_To_Cancel = FALSE;
  Wait_Acquire_slock(Mutex_Lock);
#endif

  return TRUE;
}


/* Attemps to enter into the remote mutual exclusion */
bool_t apll_enter_mutex_remote(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL) {
    if ((h->remote_agent != NULL) && (h->remote_agent != w)) {
      Safe_To_Cancel = FALSE;
      Wait_Acquire_slock(Mutex_Lock_Of(h->remote_agent));
    }
  }
#endif

  return TRUE;
}


/* Exits from the mutual exclusion */
bool_t apll_exit_mutex(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL && h->exec_state >= 0 && h->exec_state < NUMBER_STATES) {
    if ((h->agent != NULL) && (h->agent != w)) {
      Release_slock(Mutex_Lock_Of(h->agent));
      Safe_To_Cancel = TRUE;
    }
  }
#endif

  return TRUE;
}


/* Exits from the local mutual exclusion */
bool_t apll_exit_mutex_self(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  Release_slock(Mutex_Lock);
  Safe_To_Cancel = TRUE;
#endif

  return TRUE;
}


/* Exits from the remote mutual exclusion */
bool_t apll_exit_mutex_remote(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  par_handler_t *h = read_handler(X(0));

  if (h != NULL) {
    if ((h->remote_agent != NULL) && (h->remote_agent != w)) {
      Release_slock(Mutex_Lock_Of(h->remote_agent));
      Safe_To_Cancel = TRUE;
    }
  }
#endif

  return TRUE;
}


#if defined(ANDPARALLEL)
void clean_stats(l,p)
     SLOCK l;
     int_par_t *(*p);
{
  int_par_t *aux;

  Wait_Acquire_slock(l);
  while ((*p) != NULL) {
    aux = (*p);
    (*p) = (*p)->prev;
    if ((*p) != NULL)
      (*p)->next = NULL;
    checkdealloc((tagged_t *)aux,sizeof(int_par_t));
  }
  Release_slock(l);
}
#endif


/* Resets the value of statistical measures for nondet parallel
   programs */
bool_t apll_clean_measures(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  npargoals = 0;
  clean_stats(npargoalstaken_l,&npargoalstaken);
  clean_stats(nlocalbacktr_l,&nlocalbacktr);
  clean_stats(nrembacktr_top_l,&nrembacktr_top);
  clean_stats(nrembacktr_trapped_l,&nrembacktr_trapped);

#if defined(Solaris)
  double_par_t *aux;
  while (Total_Suspending_Time != NULL) {
    aux = Total_Suspending_Time;
    Total_Suspending_Time = Total_Suspending_Time->prev;
    if (Total_Suspending_Time != NULL)
      Total_Suspending_Time->next = NULL;
    checkdealloc((tagged_t *)aux,sizeof(double_par_t));
  }
  Suspending_Time_Cont = gethrtime();
  worker_t *aux_w = Next_Wam_Of(w);
  while (aux_w != w) {
    while (Total_Suspending_Time_Of(aux_w) != NULL) {
      aux = Total_Suspending_Time_Of(aux_w);
      Total_Suspending_Time_Of(aux_w) = Total_Suspending_Time_Of(aux_w)->prev;
      if (Total_Suspending_Time != NULL)
  	Total_Suspending_Time_Of(aux_w)->next = NULL;
      checkdealloc((tagged_t *)aux,sizeof(double_par_t));
    }
    Suspending_Time_Cont_Of(aux_w) = gethrtime();
    aux_w = Next_Wam_Of(aux_w);
  }
#endif
#endif

  return TRUE;
}


#if defined(ANDPARALLEL)
void print_stats(l,p,average,sigma)
     SLOCK l;
     int_par_t *p;
     float (*average);
     float (*sigma);
{
  int_par_t *aux;
  int n=0;

  Wait_Acquire_slock(l);
  aux = p;
  (*average) = 0;
  n = 0;
  while (aux != NULL) {
    n++;
    (*average)+=aux->value;
    aux = aux->prev;
  }
  if (n>0) (*average) = (*average)/n; else (*average) = 0;
  aux = p;
  (*sigma) = 0;
  n = 0;
  while (aux != NULL) {
    n++;
    (*sigma)+=(aux->value-(*average))*(aux->value-(*average));
    aux = aux->prev;
  }
  if (n>0) (*sigma) = sqrt((*sigma)/n); else (*sigma) = 0;
  Release_slock(l);
}
#endif


/* Prints the value of statistical measures for nondet parallel
   programs */
bool_t apll_print_measures(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  float average = 0;
  float sigma = 0;

  // Parallel goals
  Wait_Acquire_slock(npargoals_l);
  printf("\nParallel goals: %d\n",npargoals);
  Release_slock(npargoals_l);

  // Parallel goals taken
  print_stats(npargoalstaken_l,npargoalstaken,&average,&sigma);
  printf("Parallel goals taken by other agents:\n");
  printf("   Average: %lf\n",average);
  printf("   sigma:   %lf\n",sigma);

  // Times of local backtracking performed
  print_stats(nlocalbacktr_l,nlocalbacktr,&average,&sigma);
  printf("Parallel goals backtracked locally:\n");
  printf("   Average: %lf\n",average);
  printf("   sigma:   %lf\n",sigma);

  // Times of remote backtracking performed on top of stack
  print_stats(nrembacktr_top_l,nrembacktr_top,&average,&sigma);
  printf("Parallel goals backtracked remotely on top of stack:\n");
  printf("   Average: %lf\n",average);
  printf("   sigma:   %lf\n",sigma);

  // Times of remote backtracking over trapped goals
  print_stats(nrembacktr_trapped_l,nrembacktr_trapped,&average,&sigma);
  printf("Parallel goals backtracked remotely and trapped:\n");
  printf("   Average: %lf\n",average);
  printf("   sigma:   %lf\n",sigma);

#if defined(Solaris)
  // Suspending time of each agent
  double_par_t *aux = Total_Suspending_Time;
  int n = 0;
  double av_time = 0.0, av_n = 0.0;
  while (aux != NULL) {
    n++;
    av_time+=aux->time;
    av_n+=aux->n;
    aux = aux->prev;
  }
  if (n>0) {
    av_time = av_time/n;
    av_n = av_n/n;
  }
  else {
    av_time = 0.0;
    av_n = 0.0;
  }
  printf("Suspending time of WAM %x: %lf\n",(int)w,av_time);
  printf("   Number of times suspended: %lf\n",av_n);
  worker_t *aux_w = Next_Wam_Of(w);
  while (aux_w != w) {
    aux = Total_Suspending_Time_Of(aux_w);
    n = 0;
    av_time = 0.0;
    av_n = 0.0;
    while (aux != NULL) {
      n++;
      av_time+=aux->time;
      av_n+=aux->n;
      aux = aux->prev;
    }
    if (n>0) {
      av_time = av_time/n;
      av_n = av_n/n;
    }
    else {
      av_time = 0.0;
      av_n = 0.0;
    }
    printf("Suspending time of WAM %x: %lf\n",(int)aux_w,av_time);
    printf("   Number of times suspended: %lf\n",av_n);
    aux_w = Next_Wam_Of(aux_w);
  }
#endif
#endif
  
  return TRUE;
}


#if defined(ANDPARALLEL)
void new_stats(l,p)
     SLOCK l;
     int_par_t *(*p);
{
  int_par_t *aux;

  Wait_Acquire_slock(l);
  aux = (int_par_t *)checkalloc(sizeof(int_par_t));
  aux->prev = (*p);
  aux->next = NULL;
  aux->value = 0;
  if ((*p) != NULL)
    (*p)->next = aux;
  (*p) = aux;
  aux = NULL;
  Release_slock(l);
}
#endif


/* Prepares the statistical values for a new execution */
bool_t apll_new_measure(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  measure = TRUE;

  npargoals = 0;
  new_stats(npargoalstaken_l,&npargoalstaken);
  new_stats(nlocalbacktr_l,&nlocalbacktr);
  new_stats(nrembacktr_top_l,&nrembacktr_top);
  new_stats(nrembacktr_trapped_l,&nrembacktr_trapped);

#if defined(Solaris)
  worker_t *aux_w = Next_Wam_Of(w);
  double_par_t *aux =
    (double_par_t *)checkalloc(sizeof(double_par_t));
  Suspending_Time_Cont = gethrtime();
  aux->prev = Total_Suspending_Time;
  aux->next = NULL;
  aux->time = 0.0;
  aux->n = 0;
  if (Total_Suspending_Time != NULL)
    Total_Suspending_Time->next = aux;
  Total_Suspending_Time = aux;
  while (aux_w != w) {
    Suspending_Time_Cont_Of(aux_w) = gethrtime();
    aux = (double_par_t *)checkalloc(sizeof(double_par_t));
    aux->prev = Total_Suspending_Time_Of(aux_w);
    aux->next = NULL;
    aux->time = 0.0;
    aux->n = 0;
    if (Total_Suspending_Time_Of(aux_w) != NULL)
      Total_Suspending_Time_Of(aux_w)->next = aux;
    Total_Suspending_Time_Of(aux_w) = aux;
    aux_w = Next_Wam_Of(aux_w);
  }
#endif
#endif

  return TRUE;
}


/* Increments the number of times that local backtracking has been
   performed over nondeterministic parallel goals */
bool_t apll_incr_num_local_backtr(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  Wait_Acquire_slock(nlocalbacktr_l);
  if (nlocalbacktr != NULL && measure) (nlocalbacktr->value)++;
  Release_slock(nlocalbacktr_l);
#endif

  return TRUE;
}


/* Avoids measuring */
bool_t apll_not_measure(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  measure = FALSE;
#endif

  return TRUE;
}


/* ***************************************************************** */

bool_t nd_environment_protection_c(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  pop_choicept(Arg);
  return FALSE;
#endif

  return TRUE;
}

/* Initialization procedure */
bool_t init(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL)
  address_nd_environment_protection_c = def_retry_c(nd_environment_protection_c,0);

  fifo             = init_atom_check("fifo");
  lifo             = init_atom_check("lifo");
  det              = init_atom_check("det");
  nagents = 1;
#endif
  return TRUE;
}

