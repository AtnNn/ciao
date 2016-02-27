/* Copyright (C) 2007,2008 UPM-CLIP */

#include "datadefs.h"
#include "support.h"

#include <sys/time.h>

#include "visandor.h"


/* ***************************************************************** */
/*                       VISANDOR PRIMITIVES                         */
/* ***************************************************************** */


/* Marks the execution of the local goal as started (VisAndOr event) */
bool_t apll_mark_start_goal_local(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL) && defined(VISANDOR)
  EVENT(START_GOAL, Pcall_Level(w), 0);
#endif

  return TRUE;
}


/* Marks the execution of the goal as started (VisAndOr event) */
bool_t apll_mark_start_goal(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL) && defined(VISANDOR)
  par_handler_t *h = (par_handler_t *) read_handler(X(0));

  if (h != NULL)
    EVENT(START_GOAL, h->ppf, 1);
#endif

  return TRUE;
}


/* Marks the execution of the local goal as finished (VisAndOr
   event) */
bool_t apll_mark_finish_goal_local(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL) && defined(VISANDOR)
  EVENT(FINISH_GOAL, Pcall_Level(w), 0);
#endif

  return TRUE;
}


/* Marks the execution of the goal as finished (VisAndOr event) */
bool_t apll_mark_finish_goal(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL) && defined(VISANDOR)
  par_handler_t *h = (par_handler_t *) read_handler(X(0));

  if (h != NULL)
    EVENT(FINISH_GOAL, h->ppf, 1);
#endif

  return TRUE;
}


/* Marks a join (VisAndOr event) */
bool_t apll_mark_join(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL) && defined(VISANDOR)
  EVENT(JOIN, Pcall_Level(w), 2);
  (Pcall_Level(w))--;
#endif

  return TRUE;
}


/* Starts the event trace */
bool_t apll_start_event_trace(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL) && defined(VISANDOR)
  worker_t *aux = w;

#if !defined(USCLK_EXISTS) && !defined(NSCLK_EXISTS)
  // Initialize the count of calls to usertime(). MCL.
  count_calls = 0;
#endif

  time_at_event_start = (float)0;
  time_at_event_start = (TIME)/1e6;
  do {
    Pcall_Level(aux) = (ENG_INT)aux;
    aux = Next_Wam_Of(aux);
  }
  while (aux!=w);
  gen_event_file = TRUE;
  EVENT(START_TIME,Pcall_Level(w),0);
  EVENT(AGENT_BUSY,0,nacagents);
#if defined(DEBUG)
  if (gen_event_file)
    printf("Events started.\n");
#endif

#endif
  return TRUE;
}


/* Stops the event trace */
bool_t apll_stop_event_trace(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL) && defined(VISANDOR)
  EVENT(AGENT_IDLE,0,nacagents);
  EVENT(STOP_TIME,Pcall_Level(w),0);
#if defined(DEBUG)
  if (gen_event_file)
    printf("Events stopped.\n");
#endif
  gen_event_file = FALSE;
#endif
  return TRUE;
}


/* Saves all events to file in chronological order, clear event tables
   inefficient, but it works */
bool_t apll_save_event_trace(Arg)
     Argdecl;
{
#if defined(ANDPARALLEL) && defined(VISANDOR)
  stream_node_t *stream;
  FILE * event_file;
  visandor_event_t * eventptr;
  int agent;
  worker_t ** current_event;
  int min_ts_agent;
  unsigned long min_tstamp;
  int nagents_finished;
  worker_t *aux = w;

  stream = (stream_node_t *) stream_to_ptr(X(0), 'x');
  if (stream==NULL)
    SERIOUS_FAULT("save_event_trace/1: bad stream argument.");
  event_file = stream->streamfile;
  fprintf(event_file, "0\n");  // 0 means AND parallelism
  current_event =
    (worker_t **) checkalloc(nagents * sizeof(worker_t *));

  // Termination
  nagents_finished = 0;

  // Setup current_event pointers
  // Make timestamp of last event large so that we don't output it
  agent = 0;
  do {
    current_event[agent] = aux;
    (NextEvent(aux))->timestamp = -1;  // Largest unsigned number
    if (FirstEvent(aux) == NextEvent(aux))
      nagents_finished++;
    agent++;
    aux = Next_Wam_Of(aux);
  }
  while (aux!=w);

  do {
    // Find first (min-tstamp) of the current events
    for (min_ts_agent = 0,
           min_tstamp = (FirstEvent((current_event[0])))->timestamp,
	   agent = 1;
	 agent < nagents;
	 agent++) {
      if ( (FirstEvent((current_event[agent])))->timestamp < min_tstamp ) {
	min_ts_agent = agent;
	min_tstamp = (FirstEvent((current_event[agent])))->timestamp;
      }
    }
    // Output it
    eventptr = (FirstEvent((current_event[min_ts_agent])))++;
    fprintf(event_file, "%8lu %d %X %d %X %d \n",
	    eventptr->timestamp, eventptr->evtype, eventptr->ppf,
	    eventptr->nfork, eventptr->wam, min_ts_agent);
    // If we have finished all events for this agent, record it
    if ( (FirstEvent((current_event[min_ts_agent]))) ==
	 (NextEvent((current_event[min_ts_agent]))) ) {
      nagents_finished++;
    }
  }
  while ( nagents_finished < nagents );

  // Reset event tables
  aux = Next_Wam_Of(w);
  do {
    NextEvent(aux) = FirstEvent(aux);
    aux = Next_Wam_Of(aux);
  }
  while (aux!=w);

  return TRUE;
#else
  fprintf(stderr, "[Warning: compiled with AND_PARALLEL_EXECUTION option");
  fprintf(stderr, " not set to \"visandor\", no trace produced]");
  return TRUE;
#endif
}


