#include "datadefs.h"
#include "support.h"
#include "threads.h"
#include "task_areas.h"
/*#include "locks.h"*/

/* declarations for global functions accessed here */

#include "wam_defs.h"
#include "prolog_tasks_defs.h"
#include "tasks_defs.h"
#include "startgoal_defs.h"
#include "nondet_defs.h"
#include "main_defs.h"
#include "term_support_defs.h"

/* local declarations */

int killing_threads = FALSE;  /* Set to TRUE when killing other threads to
                                 disable fast spawning of new threads. */
/* To do:
   set/reset goal_table[].wam in startgoal routines
   change names according to concurrency.pl
 */

 /* POSIX defines a maximum (_PTHREAD_THREADS_MAX) on the number of threads
    per process --- 64, I think .  Implementations can go beyond this
    number.  I will allow 1024 simultaneous threads.  After the death of a
    thread, more can (if the implementation supports it) be created*/

#define MAXWORKERS 1024

worker_entry goal_table[MAXWORKERS];
static int next_available_id = 0;  /* "0" will be the initial goal -- locked */

void init_worker_entry_table()
{
  int i;
  for (i = 0; i < MAXWORKERS; i++)
    goal_table[i].worker = NULL;
}

#define NEXT_ID(i) i = (i+1) % MAXWORKERS

int get_new_goal_id()
{
#if defined(DEBUG)
  int wraparound_id;
#endif

  Wait_Acquire_lock(worker_id_pool_l);

#if defined(DEBUG)
  wraparound_id = next_available_id;
#endif
  while(goal_table[next_available_id].worker) {
    NEXT_ID(next_available_id);
#if defined(DEBUG)
    if (next_available_id < wraparound_id)
      fprintf(stderr, "***** Running out of worker IDs (and wrapping around)!!!!\n");
#endif
  }
 /* Signal that an entry is being used before having a proper worker for it,
    so that no other thread can steal it */
  goal_table[next_available_id].worker = (wrb_state_p)1;
  Release_lock(worker_id_pool_l);
  return next_available_id;
}


wrb_state_p init_first_worker_entry()
{
  goal_table[0].thread_id = Thread_Id;
  goal_table[0].goal_id   = 0;
  goal_table[0].action   = NO_ACTION;    /* Special case for first goal */
  return gimme_a_new_worker(goal_table);
}

/* Fill in the table of workers */


void make_worker_entry_free(we)
     worker_entry_p we;
{
  Argdecl;

  Arg = we->worker->worker_registers;
  remove_link_chains(&TopConcChpt, InitialNode);
  make_worker_free(we->worker);
  we->worker = (wrb_state_p)NULL;

  /* Possibly pending concurrent calls (which have still an associated
     choicepoint) have to be removed.  Take this into account! MCL. */
  }

static int worker_from_WAM(Arg)
     Argdecl;
{
  int i = 0;
  while ((i < MAXWORKERS) && 
         (goal_table[i].worker->worker_registers != Arg))
    i++;
  if (i == MAXWORKERS)
    MAJOR_FAULT("Thread ID not found")
  else
    return i;
}



BOOL prolog_kill_thread(Arg)
     Argdecl;
{
  wrb_state_p worker_to_kill;
  int worker_index;

  DEREF(X(0), X(0));
  if (!TagIsSmall(X(0)))
    {BUILTIN_ERROR(TYPE_ERROR(NUMBER), X(0), 1);}
  else {
    worker_index = GetSmall(X(0));
    worker_to_kill = goal_table[worker_index].worker;

    if (!worker_to_kill)
      MAJOR_FAULT("Trying to kill a non-existent worker")

    if (worker_to_kill->state == IDLE)
      USAGE_FAULT("Trying to kill an idle worker")

    if (worker_to_kill->state == WORKING) {
      if (kill_thread(worker_to_kill))
        MAJOR_FAULT("Error killing a worker")
    } 
    make_worker_entry_free(&goal_table[worker_index]);
    return TRUE;
  }
}


BOOL prolog_kill_other_threads(Arg)
     Argdecl;
{
  int i;
  THREAD_T myself;
  BOOL worker_killed;

  myself = Thread_Id;
  killing_threads = TRUE;                      /* disable thread creation */
  do {
    worker_killed = FALSE;
    for (i = 0; i < MAXWORKERS; i++) {
      Wait_Acquire_lock(worker_id_pool_l);
      if (goal_table[i].worker &&
          (goal_table[i].thread_id != myself)) {
        if (goal_table[i].worker->state != IDLE)
          kill_thread(goal_table[i].worker);
        goal_table[i].worker->state = IDLE;
        goal_table[i].worker = NULL;
        worker_killed = TRUE;
      }
      Release_lock(worker_id_pool_l);
    }
  } while(worker_killed);
  killing_threads = FALSE;
  return TRUE;
}


/* Wait for a goal to finish */

BOOL prolog_join_goal(Arg)
     Argdecl;
{
  THREAD_T thread_id;

  DEREF(X(0), X(0));
  if (!TagIsSmall(X(0)))
    {BUILTIN_ERROR(TYPE_ERROR(NUMBER), X(0), 1);}
  else
#if defined(DEBUG)
    if (debug_threads)
      printf("About to join goal %ld\n", GetSmall(X(0)));
#endif
  thread_id = goal_table[GetSmall(X(0))].thread_id; 
  if (thread_id != Thread_Id)      /* Waiting for us makes no sense to me */
    Thread_Join(thread_id);                            /*Join, remove res.*/ 
#if defined(DEBUG)
  if (debug_threads)
    printf("Join goal %ld joined\n", GetSmall(X(0)));
#endif
  return TRUE;
}


 /* Unifies its argument with the worker number of this task. */

BOOL prolog_thread_self(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
  return cunify(Arg, X(0), MakeSmall(worker_from_WAM(Arg)));
}


 /* Prints info about the status of the launched tasks and memory areas used
    by them. */

BOOL prolog_tasks_status(Arg)
     Argdecl;
{
  print_task_status(Arg);
  return TRUE;
}


#define IS_CALLABLE(What) IsVar(What) || TagIsSmall(What) || TagIsLarge(What)

#define ENSURE_CALLABLE(What, ArgNum)   \
if (IS_CALLABLE(What))  \
  BUILTIN_ERROR(TYPE_ERROR(CALLABLE), What, ArgNum)


#if defined(UNDEFINED)
#if defined(THREADS)

/* Starting goals: when we have to copy a goal locally (locally replicating
   variables), we need a lock to make sure that our son has copied
   completely the goal before we proceed.  Otherwise we may bind/unbind the
   variables before the copy is complete.  We are using a global lock, which
   in extreme conditions would impose some contention to the fast creation
   of new tasks: a local lock would be better.

   In fact, this is still not right: since we can have threads which can
   access see the variables in the goal being copied, these threads (which
   are not necessarily synchronized with this one) may change the variables
   being copied.  In other words, this copying is atomic with respect to the
   father of the new thread, but not with respect to the whole system.  That
   seems to be too costly.  In any case, we deem that synchronization to be
   the users task. */


/* launch_goal(GoalStructure, Sharing, GoalId).  This predicate should never
   be called directly by the user, so some possible errors are not checked.  */

BOOL prolog_launch_goal_plus(Arg)
     Argdecl;
{
  TAGGED         goal_structure;
  TAGGED         sharing_type;
  TAGGED         requested_id;
  int            new_goal;
  worker_entry_p new_worker;

  if (killing_threads){
    return TRUE;
  } else {
    new_goal = get_new_goal_id();
    new_worker = &goal_table[new_goal];
    new_worker->action = NO_ACTION;
    DEREF(goal_structure, X(0));         /* X(0) points to goal structure */

  /* X(0) is not directly generated by the user, so no error checks on it.
     However, the contents of the structure must be callable terms at
     runtime.  The arg. number in the errors refer to the predicates
     available to the user, and not to launch_goal_plus/3 */
    
    if (Arity(TagToHeadfunctor(goal_structure)) == 1){   /* A single goal */
      DEREF(new_worker->goal_or_goal_and_conts, CTagToArg(goal_structure, 1));
      ENSURE_CALLABLE(new_worker->goal_or_goal_and_conts, 1);
    } else {                                    /* Goal and continuations */
      new_worker->goal_or_goal_and_conts = goal_structure;
      /*ENSURE_CALLABLE(new_worker->g.gas.on_failure,3);
        ENSURE_CALLABLE(CTagToCar(new_worker->g.gas.goal_and_success),1); */
      new_worker->action |= HAS_CONTINUATION;
    }

    DEREF(sharing_type, X(1));
    if (sharing_type == atom_share) new_worker->action |= SHARES_STRUCTURE;
    DEREF(requested_id, X(2));
    if (requested_id != atom_nil)   new_worker->action |= REQUESTED_ID;
    
    goal_table[new_goal].goal_id = new_goal;
    Wait_Acquire_lock(launch_goal_l);                  /* Lock new worker */
#if defined(DEBUG)
    if(debug_threads)
      printf("Wam %x starts goal %d with actions %d\n",
             (int)Arg, new_goal, new_worker->action);
#endif

    if (requested_id)
      {Thread_Create_GoalId(startgoal,
                       (void *)(&goal_table[new_goal]),
                            &(goal_table[new_goal].thread_id));}
    else
      {Thread_Create_NoGoalId(startgoal,
                         (void *)(&goal_table[new_goal]),
                              &(goal_table[new_goal].thread_id));}

    Wait_Acquire_lock(launch_goal_l); /* Wait for completion of startgoal */
    Release_lock(launch_goal_l);       /* and leave the lock as it was... */
#if defined(DEBUG)
    if(debug_threads)
      printf("Wam %x started goal %d at thread %d\n",
             (int)Arg, new_goal, (int)goal_table[new_goal].thread_id);
#endif
    if (requested_id == atom_nil)
      return TRUE;
    else
      return cunify(Arg, X(2), MakeInteger(Arg, new_goal));
  }
}



#if defined(UNDEFINED)
/* Backtrack over the worker ID passed as first argument */

BOOL prolog_backtrack_goal(Arg)
     Argdecl;
{

  int worker_id;
  worker_entry_p worker_entry;
  wrb_state_p    worker_str;

  DEREF(X(0), X(0));
  if (!IsNumber(X(0)))
    BUILTIN_ERROR(TYPE_ERROR(NUMBER), X(0), 1);

  worker_id = GetInteger(X(0));
  
  if (worker_id > MAXWORKERS)
    MAJOR_FAULT("Goal Id non existent (number too big)")

  worker_entry = &goal_table[worker_id];
  
  if (!(worker_str = worker_entry->worker))
    MAJOR_FAULT("Trying to backtrack over a finished or non-existent goal")

  if (worker_str->state != WAITING)
    MAJOR_FAULT("Trying to backtrack over a worker not in waiting state")

/* 
   Then, we have a worker which is waiting. We ask for backtracking on
   the WAM; after return, if we hit the initial ghost choicepoint,
   then it means that no solution was returned by this call.  Hence, 
   we fail.
*/

  worker_entry->action = worker_str->action = BACKTRACKING;
  wam(worker_str->worker_registers, worker_str);
  if (worker_str->worker_registers->node ==
      worker_str->node_at_entry){
    make_worker_entry_free(worker_entry);
    return FALSE;
  } else {
    worker_str->state = WAITING;
    return TRUE;
  }    
}
#endif                                                       /* UNDEFINED */

#else                                                          /* THREADS */

BOOL prolog_backtrack_goal(Arg)
     Argdecl;
{ USAGE_FAULT("backtrack_goal/1 not available in this version");}

BOOL prolog_launch_goal_plus(Arg)
     Argdecl;
{ USAGE_FAULT("launch_goal calls not available in this version"); }

#endif                                                         /* THREADS */
#endif                                                       /* UNDEFINED */



BOOL prolog_release_goal(Arg)
     Argdecl;
{
  worker_entry_p worker_entry;

  DEREF(X(0), X(0));

  if (!IsNumber(X(0)))
    BUILTIN_ERROR(TYPE_ERROR(NUMBER), X(0), 1);

  worker_entry = &goal_table[GetInteger(X(0))];
  if (!(worker_entry->worker))
    MAJOR_FAULT("Trying to release a non-registered or already released worker")
  else if (worker_entry->worker->state != WAITING)
    MAJOR_FAULT("Trying to release a non-waiting worker")

  make_worker_entry_free(worker_entry);
  return TRUE;
}



BOOL prolog_eng_call(Arg)
     Argdecl;
{
  int            new_goal_id;
  worker_entry_p goal_info;
  wrb_state_p    worker_space;
  BOOL           create_thread = FALSE;
  BOOL           create_wam    = TRUE;
  BOOL           keep_stacks;

  if (killing_threads) return TRUE;
  
  /* Do we have to create a wam, or wait for a new one? */
  DEREF(X(1), X(1));
  if ((X(1) == atom_wait) || X(1) == atom_create)
    create_wam = TRUE;                           /* By now, always create */
  else
    return FALSE;

  /* Do we have to create a thread, or wait for a new one? */
  DEREF(X(2), X(2));
  if ((X(2) == atom_wait) || X(2) == atom_create)  /* distinguish later */
    create_thread = TRUE;
  else 
    if (X(2) == atom_self)
      create_thread = FALSE;
    else return FALSE;
  
  /* Make sure we are calling a callable term! */

  DEREF(X(0), X(0));
  ENSURE_CALLABLE(X(0), 1);

  DEREF(X(4), X(4));
  keep_stacks = (X(4) == atom_true);

  if (create_wam) {                   /* Always TRUE, now --- but beware! */
    new_goal_id = get_new_goal_id();       /* Grab a new goal id --- easy */
           /* Get stacks to work on---this is where we might have to wait */
    if ((worker_space = look_for_a_free_worker()) == NULL) 
      worker_space = attach_me_to_wrb_state_list(create_and_init_wam());
  }
                                    /* Got goal id + memory space, go on! */
  goal_info = &goal_table[new_goal_id];
  goal_info->goal          = X(0);
  goal_info->worker        = worker_space;
  goal_info->create_thread = create_thread;
  worker_space->goal_id = goal_info->goal_id = new_goal_id;
  worker_space->action  = goal_info->action  = 
    keep_stacks ? KEEP_STACKS : NO_ACTION;

  /* We are not sharing variables, so we always copy */

  { 
    /* Incredible hack: we set X(0) in the new worker to point to the goal
       structure copied in the memory space of that new worker. We can use
       the already existent macros just by locally renaming the Arg (c.f.,
       "w") worker structure pointer. */

    Argdecl = worker_space->worker_registers;
    DEREF(X(0), cross_copy_term(Arg, goal_info->goal));
  }
  
  if (create_thread)
    Wait_Acquire_lock(launch_goal_l);                  /* Lock new worker */

#if defined(DEBUG)
  if(debug_threads)
    printf("Wam %x starts goal %d with actions %d\n",
           (int)Arg, new_goal_id, goal_info->action);
#endif
                                                    /* Always request ID! */
  if (create_thread)
    {Thread_Create_GoalId(startgoal_simp,
                         (void *)(goal_info),
                          &(goal_info->thread_id));}
  else
    startgoal_simp((void *)(goal_info));


  if (create_thread){
    Wait_Acquire_lock(launch_goal_l); /* Wait for completion of startgoal */
    Release_lock(launch_goal_l);                       /* Lock new worker */
  }    

  return cunify(Arg, X(3), MakeInteger(Arg, new_goal_id));
}


void *make_backtracking(wo)
     void *wo;
{
  wrb_state_p worker_space = (wrb_state_p)wo;
  worker_space->thread_id = Thread_Id;
  wam(worker_space->worker_registers, worker_space);
  if (worker_space->worker_registers->node == worker_space->node_at_entry)
    make_worker_entry_free(&goal_table[worker_space->goal_id]);
  else
    worker_space->state = WAITING;
  return NULL;                               /* Avoid compiler complaints */
}

/* Backtrack over the worker ID passed as first argument */

BOOL prolog_eng_backtrack(Arg)
     Argdecl;
{

  int            goal_id;
  worker_entry_p goal_info;
  wrb_state_p    worker_space;
  BOOL           create_thread;

  if (killing_threads) return TRUE;

  DEREF(X(0), X(0));
  if (!IsNumber(X(0))){
    BUILTIN_ERROR(TYPE_ERROR(NUMBER), X(0), 1);
  }

  goal_id = GetInteger(X(0));
  if (goal_id > MAXWORKERS){
    MAJOR_FAULT("Goal Id non existent (number too big)")
  }

  DEREF(X(1), X(1));
  if ((X(1) == atom_wait) || X(1) == atom_create)    /* distinguish later */
                              /* Check threads, wait if no more available */
    create_thread = TRUE;
  else 
    if (X(1) == atom_self)
      create_thread = FALSE;
    else return FALSE;

  Wait_Acquire_lock(backtrack_goal_l);

  goal_info = &goal_table[goal_id];
  if (!(worker_space = goal_info->worker)) {
    Release_lock(backtrack_goal_l);
    MAJOR_FAULT("Trying to backtrack over a finished or non-existent goal")
  }

  if (worker_space->state != WAITING){
    Release_lock(backtrack_goal_l);
    MAJOR_FAULT("Trying to backtrack over a worker not in waiting state")
  }

  goal_info->create_thread = create_thread;

  /*
    Then, we have a worker which is waiting. We ask for backtracking on
    the WAM; after return, if we hit the initial ghost choicepoint,
    then it means that no solution was returned by this call.  Hence, 
    we fail.
  */
  
  worker_space->state = WORKING;
  goal_info->action = worker_space->action = BACKTRACKING;

  Release_lock(backtrack_goal_l);

  if (create_thread)
    {Thread_Create_GoalId(make_backtracking,
                         (void *)(worker_space),
                          &(goal_info->thread_id));}
  else {
    wam(worker_space->worker_registers, worker_space);
    if (worker_space->worker_registers->node ==
        worker_space->node_at_entry){
      make_worker_entry_free(goal_info);
      return FALSE;
    } else {
      worker_space->state = WAITING;
      return TRUE;
    }
  }    
  return TRUE;           /* thread-delegated backtracking always suceeds  */
}



BOOL prolog_eng_cut(Arg)
     Argdecl;
{
  int            goal_id;
  worker_entry_p goal_info;
  wrb_state_p    worker_space;
  
  /*
    set w->node  (that is what DOCUT does), call fail...
    look at metacut, remember to delete the conc. data structures...
  */

  DEREF(X(0), X(0));
  if (!IsNumber(X(0)))
    BUILTIN_ERROR(TYPE_ERROR(NUMBER), X(0), 1);
  goal_id = GetInteger(X(0));
  goal_info = &goal_table[goal_id];
  worker_space = goal_info->worker;

  if (!(worker_space = goal_info->worker))
    MAJOR_FAULT("Trying to cut a finished or non-existent goal")

  if (worker_space->state != WAITING)
    MAJOR_FAULT("Trying to cut a worker not in waiting state")

  goal_info->action = worker_space->action = BACKTRACKING;

  {
    Argdecl = worker_space->worker_registers;
    w->node = InitialNode;            /* DOCUT to the initial choicepoint */
            /* For concurrent goals, erase the concurrent data structures */
    if (ChoiceYounger(TopConcChpt, w->next_node))
      remove_link_chains(&TopConcChpt, w->next_node);
  }

  wam(worker_space->worker_registers, worker_space);

  return TRUE;
}


/* For this one: we have to deallocate al the WAM areas, etc; it is not a
   lot of work, just cumbersome... I have other things to do now! */
/*
BOOL prolog_eng_clean(Arg)
{
  int goal_id;

  Wait_Acquire_lock(worker_id_pool_l);
  for (goal_id=0; goal_id < MAXWORKERS; goal_id++){
    if (goal_table[goal_id].worker && 
        (goal_table[goal_id].worker->state == IDLE))
      ;
  }
  Release_lock(worker_id_pool_l);
}
*/


