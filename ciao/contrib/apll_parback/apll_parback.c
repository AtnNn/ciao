/* Copyright (C) 2007,2008 UPM-CLIP */

#include "apll_parback.h"
#include "apll_parback_initial.c" // + move_execution_top + cancellation
#include "apll_parback_aux.c"       
#include "apll_parback_exec_limits.c"
#include "apll_parback_exec_state.c"
#include "apll_parback_mutex.c"


/*****************************************************************/
/*            ANDPROLOG LOW-LEVEL SUPPORT PRIMITIVES             */
/*****************************************************************/

/* Succeeds if the handler has not been picked up yet by another agent
   and fails otherwise */
bool_t apll_parback_goal_available(Arg)
     Argdecl;
{
#if defined(PARBACK)
  if (show_all) { printf("%p Entering goal_available\n",Arg); fflush(stdout); }
  DEREF(X(0),X(0));
  par_goal_t *h = (par_goal_t *)GetInteger(X(0));
  if (h->remote_agent == NULL)
    {
      h->remote_agent = Arg;
      h->exec_state = REM_EXECUTING; 
      remove_handler_from_goal_list(Arg,h);
      //add_handler_to_back_goal_list(Arg,h); //it is not needed, sequential backtracking
      if (show_all) { printf(" %p Exiting goal_available OK\n",Arg); fflush(stdout); }
      return TRUE;
    }
  if (show_all) { printf("%p Exiting goal_available NO\n",Arg); fflush(stdout); }
  return FALSE;
#endif

  return TRUE;
}

/* Finds a handler associated to a goal from the goal list */
/* of some agent or ready to be backtracked. Mutex_self is taken. */
bool_t apll_parback_get_handler_to_executed(Arg)
     Argdecl;
{
#if defined(PARBACK)
  if (show_all) { printf("%p Entering get handler\n",Arg); fflush(stdout); }
  bool_t found_goal = FALSE;
  worker_t *aux;
  par_goal_t *h;
  goal_entry_t *gl;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  Wait_Acquire_lock(Waiting_For_Work_Lock);
  for (aux = Next_Wam_Of(Arg); (aux != Arg) && !found_goal; aux = Next_Wam_Of(aux)) 
    {
      Wait_Acquire_slock(Mutex_Lock_Of(aux));
      if (Goal_List_Start_Of(aux) != NULL) 
	{
	  // Pick up the handler
	  if (X(0) == fifo)  // FIFO scheduling
	    h = Goal_List_Start_Of(aux)->par_goal;
	  else if (X(0) == lifo)  // LIFO scheduling
	    h = Goal_List_Top_Of(aux)->par_goal;
	  else
	    MAJOR_FAULT("Goal scheduling error in apll_parback_find_goal.");

	  if (show_lists) { printf("%p GET HANDLER: Stealing %p\n",Arg,h); fflush(stdout);}

	  // Give value to remote_agent in the handler
	  h->remote_agent = Arg;
	      
	  // Return a pointer to the remote goal in X(1)
	  Unify(X(1),MakeInteger(Arg, (tagged_t)h));
	      	  
	  // Remove handler from goal list in remote agent
	  remove_handler_from_goal_list(aux,h);
	      
	  if (show_lists) { printf("%p GET HANDLER: remove handler from goal list DONE\n",Arg); fflush(stdout);}
	  // Exit
	  found_goal = TRUE;
	}
      Release_slock(Mutex_Lock_Of(aux));
    }
  
  Wait_Acquire_slock(Mutex_Lock);
  //Con suspension aqui lo primero es ver si me han suspendido cancelado????
  //quiza no haya que meterlo en la lista de backtracking xq cambio el estado.
  if (found_goal) 
    {
      if (show_lists) { printf("%p GET HANDLER: %p stolen from available\n",Arg,h); fflush(stdout);}
      add_handler_to_back_goal_list(Arg,h);
    }
  else if (Back_Goal_List_Start != NULL)
    {
      for (gl = Back_Goal_List_Start; !found_goal; gl = gl->next)
	{
	  node_t *prev_node = ChoiceCharOffset(Arg->node,-Arg->node->next_alt->node_offset);
	  node_t *prev_prev_node = ChoiceCharOffset(prev_node,-prev_node->next_alt->node_offset);
	  if (show_lists) 
	    { 
	      printf("%p GET HANDLER: back list choices end: %p node_: %p prev-node: %p prev-prev-node: %p\n",
		     Arg,gl->par_goal->ch_end, Arg->node, prev_node, prev_prev_node);
	      fflush(stdout);
	    }
 	  if (gl->par_goal->ch_end == prev_prev_node)
	    {
	      Wait_Acquire_slock(gl->par_goal->pf->vars_l);	      
	      if (!gl->par_goal->pf->combining)
		{
		  if (show_lists) { printf("%p GET HANDLER: stealing %p from backtracking list\n",Arg,gl->par_goal); fflush(stdout);}
		  // Return a pointer to the remote goal in X(1)
		  Unify(X(1),MakeInteger(Arg, (tagged_t)gl->par_goal));
		  found_goal = TRUE;
		  Release_slock(gl->par_goal->pf->vars_l);
		  break;
		}
	      else 
		{
		  if (show_lists) { printf("%p GET HANDLER: %p is already combining\n",Arg,gl->par_goal); fflush(stdout);}
		  Release_slock(gl->par_goal->pf->vars_l);	      
		  break;
		}
	    }
	  if (gl == Back_Goal_List_Top) { printf("\nPROBLEMAS GET HANDLER\n"); fflush(stdout); break; }
	}
    }
  if (found_goal) Release_lock(Waiting_For_Work_Lock);
  else Release_slock(Mutex_Lock);
  if (show_all) { printf("%p Exiting get handler\n",Arg); fflush(stdout); }
  return found_goal;

#else
  return TRUE;
#endif
}

bool_t apll_parback_move_execution_top(Arg)
     Argdecl;
{
#if defined(PARBACK)
  //Pending: reinstalar el value trail aqui -> al crearlo en join --> VER SI YA != NULL
  //el problema son los locales!!
  Arg->node = ChoiceCharOffset(Arg->node,-Arg->node->next_alt->node_offset);
#endif

  return TRUE;
}

bool_t apll_parback_fork(Arg)
     Argdecl;
{
#if defined(PARBACK)
  if (show_all) { printf("%p Entering fork det\n",Arg); fflush(stdout); }
    
  //Create a parallel call frame
  PF_t *pf= (PF_t*) checkalloc(sizeof(PF_t));
  tagged_t list;
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(list,X(2));
  int numGoals = GetInteger(X(1));

  Init_slock(pf->vars_l);
  Wait_Acquire_slock(pf->vars_l);					
  pf->prev = Act_PF;
  pf->combining = FALSE;
  pf->inside = TRUE;
  pf->ansPending = numGoals;
  pf->failPending = numGoals;
  Release_slock(pf->vars_l);					
  pf->numGoals = numGoals;
  pf->goals = (par_goal_t *)checkalloc(numGoals*sizeof(par_goal_t));
  for (numGoals = 0; numGoals < pf->numGoals; numGoals++) 
    {
      pf->goals[numGoals].goal = HeadOfList(list);
      list = TailOfList(list);
      pf->goals[numGoals].pf = pf;
      pf->goals[numGoals].mem_size = MEMSIZE;
      pf->goals[numGoals].memory = checkalloc (MEMSIZE * sizeof(tagged_t));
      pf->goals[numGoals].value_trail = NULL;
      pf->goals[numGoals].free = 0;
      pf->goals[numGoals].array = checkalloc (TERMSIZE * sizeof(tagged_t));
      pf->goals[numGoals].firstAns = NULL;
      pf->goals[numGoals].lastComb = NULL;
      pf->goals[numGoals].nowComb = NULL;
      pf->goals[numGoals].lastAns = NULL;
      pf->goals[numGoals].ch_init = NULL;
      pf->goals[numGoals].ch_end = NULL;
      pf->goals[numGoals].trail_init = NULL;
      pf->goals[numGoals].trail_end = NULL;
      pf->goals[numGoals].combining = FALSE;
      pf->goals[numGoals].exec_state = NOT_STARTED;
      pf->goals[numGoals].agent = Arg;
      pf->goals[numGoals].remote_agent = NULL;
      pf->goals[numGoals].gle = (goal_entry_t *)checkalloc(sizeof(goal_entry_t));
      pf->goals[numGoals].gle->par_goal = pf->goals + numGoals;
      pf->goals[numGoals].list = NONE;
    }
  pf->goals[0].remote_agent = Arg;  
  
  worker_t *prev = Arg;
  for (numGoals = pf->numGoals - 1; numGoals > 0; numGoals--)
    {
      bool_t released_agent = FALSE;
      add_handler_to_goal_list(Arg,pf->goals + numGoals);
      worker_t *aux = Next_Wam_Of(prev);
      Release_slock(Mutex_Lock);
      do  //Release a suspended agent, if there is one
	{
	  Wait_Acquire_lock(Waiting_For_Work_Lock_Of(aux));
	  if (Suspended_Waiting_For_Work_Of(aux)) 
	    {
	      Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(aux));
	      released_agent = TRUE;
	      prev = Arg;
	    }
	  Release_lock(Waiting_For_Work_Lock_Of(aux));
	  aux = Next_Wam_Of(aux);
	}
      while (!released_agent && (aux != prev));
      Wait_Acquire_slock(Mutex_Lock);
    }
  

  Unify(X(0),MakeInteger(Arg, (tagged_t)pf));
  DEREF(X(0),X(0));

  //Unifying goal handler list of pf
  DEREF(list,X(3));
  int i;
  if (show_fork) { printf("%p Creating PF %p with goals: ",Arg,pf); fflush(stdout); }
  for (i = 0; i < pf->numGoals; i++)
    {
      if (show_fork) { printf("%p ",pf->goals + i); fflush(stdout); }
      Unify(list, MkPairTerm(MakeInteger(Arg,(tagged_t)(pf->goals + i)), MkVarTerm(Arg)));
      DEREF(list,list);
      list = TailOfList(list);
    }
  if (show_fork) { printf("\n"); fflush(stdout); }

  Unify(list,EMPTY_LIST);

  push_choicept(Arg,address_nd_fork_c);
  Arg->node->term[0] = X(0);

  Act_PF = pf;

  if (show_all) { printf("%p Exiting fork det\n",Arg); fflush(stdout); }
#endif

  return TRUE;
}

bool_t nd_fork_c(Arg)
     Argdecl;
{
#if defined(PARBACK)
  Wait_Acquire_slock(Mutex_Lock);
  if (show_all) { printf("%p Entering fork nondet\n",Arg); fflush(stdout); }
  //Frees a parallel call frame
  DEREF(X(0),X(0));
  PF_t *pf = (PF_t *)GetInteger(X(0));

  Wait_Acquire_slock(pf->vars_l);
  if (pf->combining || pf->failPending > 0) 
    {
      if (show_all) 
	{ 
	  printf("%p Entering fork nondet TRUE: combining %d failPending %d\n",
		 Arg,pf->combining,pf->failPending); fflush(stdout); 
	}
      Release_slock(pf->vars_l);
      return TRUE; 
    }
  Release_slock(pf->vars_l);
  Act_PF = pf->prev;

  //Freeing structures of each handler.
  int iGoals;
  for (iGoals = 0; iGoals < pf->numGoals; iGoals++) 
    {
      //Freeing the answer list of the handler
      AL_t *iAns = pf->goals[iGoals].firstAns;
      AL_t *auxAns;
      while (iAns != NULL)
	{
	  auxAns = iAns->next;
	  if (iAns->numVars > 0) checkdealloc(iAns->vars, iAns->numVars * sizeof(tagged_t));
	  checkdealloc((tagged_t*)iAns, sizeof(AL_t));
	  iAns = auxAns;
	}
      //Freeing answer memory
      checkdealloc (pf->goals[iGoals].array, TERMSIZE * sizeof(tagged_t));
      checkdealloc (pf->goals[iGoals].memory, pf->goals[iGoals].mem_size * sizeof(tagged_t));
      checkdealloc ((tagged_t*)pf->goals[iGoals].gle, sizeof(goal_entry_t));
    }

  //Freeing handlers
  checkdealloc((tagged_t*)pf->goals, pf->numGoals*sizeof(par_goal_t));
  //Freeing the parcall frame
  checkdealloc((tagged_t*)pf,sizeof(PF_t));

  pop_choicept(Arg);
  Release_slock(Mutex_Lock);
  if (show_all) { printf("%p Exiting fork nondet\n",Arg); fflush(stdout); }
  return FALSE;
#endif

  return TRUE;
}


bool_t apll_parback_join(Arg)
     Argdecl;
{
#if defined(PARBACK)
  if (show_all) { printf("%p Entering join det %p\n",Arg,Arg->trail_top); fflush(stdout); }

  //Joins the solved answers of the parcall frame X(0)
  DEREF(X(0),X(0));
  PF_t *pf = (PF_t *)GetInteger(X(0));

  Wait_Acquire_slock(pf->vars_l);
  if (!pf->combining) 
    {
      Release_slock(pf->vars_l);
      Release_slock(Mutex_Lock);
      if (show_all) { printf("%p Exiting join det NO COMBINING\n",Arg); fflush(stdout); }
      return FALSE;
    }
  pf->inside = FALSE;
  Release_slock(pf->vars_l);

  //Saving value trail
  if (show_join) { printf("JOIN %p: STRATING SaveValueTrailAndUntrailState %p\n",Arg,pf); fflush(stdout); }
  SaveValueTrailAndUntrailState(pf);
  if (show_join) { printf("JOIN %p: SaveValueTrailAndUntrailState %p DONE\n",Arg,pf); fflush(stdout); }

  Act_PF = pf->prev;

  if (show_join) { printf("JOIN %p: STARTING InitializeNowComb %p\n",Arg,pf); fflush(stdout); }
  int comb = -1;
  InitializeNowComb(pf);
  if (show_join) { printf("JOIN %p: InitializeNowComb %d %p DONE\n",Arg,comb,pf); fflush(stdout); }

  push_choicept(Arg,address_nd_join_c);

  if (show_join) { printf("JOIN %p: STARTING NowCombAnswerForEachGoalToHeap %p\n",Arg,pf); fflush(stdout); }
  NowCombAnswerForEachGoalToHeap(pf);
  if (show_join) { printf("JOIN %p: NowCombAnswerForEachGoalToHeap %p DONE\n",Arg,pf); fflush(stdout); }

  int right;
  //Looking the most right goal with uncombined answers
  for (right = pf->numGoals - 1; (right >= 0) && (pf->goals[right].nowComb == pf->goals[right].lastComb); right--)
    {
      if (pf->goals[right].combining) comb = right;
    }

  if (show_join) { printf("JOIN %p: Next answer taken in %d\n",Arg,right); fflush(stdout); }
  if (show_join) { printf("JOIN %p: PF %p PRE node->trail %p\n",Arg,pf,Arg->node->trail_top); fflush(stdout); }
  if (Alig(Arg->node->trail_top) != pf->goals[comb].nowComb->trail)
    printf("PROBLEMAS TRAIL JOIN\n");
  Arg->node->term[1] = MakeInteger(Arg, (tagged_t)right);
  if (right >= 0) Change(Arg->node->trail_top,pf->goals[right].nowComb->trail);
  if (show_join) { printf("JOIN %p: POST node->trail %p\n",Arg,Arg->node->trail_top); fflush(stdout); }
  if (show_join) { printf("JOIN %p: NODE %p var1 %p\n",Arg,Arg->node, Arg->node->term[1]); fflush(stdout); }

  //The choice point protects the generated answers (and the right number) from backtracking
  Arg->node->global_top = Arg->global_top;

  if (show_all) { printf("%p Exiting join det COMBINING\n",Arg); fflush(stdout); }

#endif

  return TRUE;
}

bool_t nd_join_c(Arg)
     Argdecl;
{
#if defined(PARBACK)
  Wait_Acquire_slock(Mutex_Lock);
  if (show_all) { printf("%p Entering join nondet %p\n",Arg,Arg->trail_top); fflush(stdout); }

  //Getting the parallel call frame
  DEREF(X(0),X(0));
  PF_t *pf = (PF_t *)GetInteger(X(0));

  if (show_join) { printf("JOIN %p: PF taken %p\n",Arg,pf); fflush(stdout); }
  DEREF(X(1),X(1));
  int right = GetInteger(X(1));

  if (show_join) { printf("JOIN %p: right taken %d from %p\n",Arg,right,Arg->node); fflush(stdout); }
  
  //all answers have been combined
  if (right < 0)
    {      
      if (show_join) { printf("JOIN %p: No more answers taken\n",Arg); fflush(stdout); }
      NoMoreCombinig(pf);
      if (show_join) { printf("JOIN %p: UntrailNowCombAnswerForEachGoal DONE\n",Arg); fflush(stdout); }

      for (right = 0; right < pf->numGoals; right++)
	if (pf->goals[right].lastComb != pf->goals[right].lastAns)
	  {
	    if (show_join) { printf("JOIN %p: More combinable answers\n",Arg); fflush(stdout); }
	    //combine a new answer
	    pf->goals[right].lastComb = pf->goals[right].lastComb->next;
	    pf->goals[right].nowComb = pf->goals[right].lastComb;
	    pf->goals[right].combining = TRUE;
	    if (show_join) { printf("JOIN %p: Reusing %d numVars\n",Arg,right,(pf)->goals[right].nowComb->numVars); fflush(stdout); } 
	    NowCombAnswerToHeap(pf->goals[right]);
	    if (show_join) { printf("JOIN %p: New combinable answers in the heap DONE\n",Arg); fflush(stdout); }
	   	    
	    //Put the first answer of the goals
	    for (right = 0; right < pf->numGoals; right++)
	      {
		if (pf->goals[right].combining) continue;
		if (show_join) { printf("JOIN %p: redoing %d %p\n",Arg,right,pf->goals[right].firstAns); fflush(stdout); }
		pf->goals[right].nowComb = pf->goals[right].firstAns;
		if (pf->goals[right].nowComb->pHeap != NULL)
		  {
		    if (show_join) 
		      { 
			printf("JOIN %p: Reusing %d numVars\n",Arg,right,(pf)->goals[right].nowComb->numVars); 
			fflush(stdout); 
		      } 
		    PutAnswerBindingsFromHeap(pf->goals[right].nowComb);
		    if (show_join) { printf("JOIN %p: PutAnswerBindingsFromHeap DONE\n",Arg); fflush(stdout); }
		  }
		else 
		  {
		    if (show_join) 
		      { 
			printf("JOIN %p: Reusing %d numVars\n",Arg,right,(pf)->goals[right].nowComb->numVars); 
			fflush(stdout); 
		      } 
		    NowCombAnswerToHeap(pf->goals[right]);
		    if (show_join) { printf("JOIN %p: NowCombAnswerToHeap DONE\n",Arg); fflush(stdout); }
		  }
		if (show_join) { printf("JOIN %p: redoing %d DONE\n",Arg,right); fflush(stdout); }
	      }	

	    if (show_join) { printf("JOIN %p: First answer of the rest of goals reinstalled DONE\n",Arg); fflush(stdout); }
	    if (show_all) { printf("%p Exiting join nondet I\n",Arg); fflush(stdout); }

	    //Looking the most right goal with uncombined answers
	    int comb;
	    for (right = pf->numGoals - 1; (right >= 0) && (pf->goals[right].nowComb == pf->goals[right].lastComb); right--)
	      {
		if (pf->goals[right].combining) comb = right;
	      }
	    
	    if (show_join) { printf("JOIN %p: Next answer taken in %d\n",Arg,right); fflush(stdout); }
	    if (show_join) { printf("JOIN %p: PF %p PRE node->trail %p\n",Arg,pf,Arg->node->trail_top); fflush(stdout); }
	    Arg->node->term[1] = MakeInteger(Arg, (tagged_t)right);
	    if (right >= 0) Change(Arg->node->trail_top,pf->goals[right].nowComb->trail);
	    else Change(Arg->node->trail_top,pf->goals[comb].nowComb->trail);
	    if (show_join) { printf("JOIN %p: POST node->trail %p\n",Arg,Arg->node->trail_top); fflush(stdout); }

	    //The choice point protects the generated answers (and the right number) from backtracking
	    Arg->node->global_top = Arg->global_top;

	    return TRUE;
	  }
      
      //If not -> all pHeap <- NULL;
      if (show_join) { printf("JOIN %p: Preparing to fail\n",Arg); fflush(stdout); }
      PutPHeapToNULL(pf);
      if (show_join) { printf("JOIN %p: PutPHeapToNULL DONE\n",Arg); fflush(stdout); }

      pop_choicept(Arg);
      Wait_Acquire_slock(pf->vars_l);
      pf->inside = TRUE;
      pf->combining = FALSE;			
      Release_slock(pf->vars_l);
      Act_PF = pf;
      int i;
      FreeValueTrailAndRedoState(pf);
      if (show_join) { printf("JOIN %p: FreeValueTrailAndRedoState DONE\n",Arg); fflush(stdout); }

      Release_slock(Mutex_Lock);
      //Pending: si estaba inside!!!
      for (i = 1; i < pf->numGoals; i++)
	if ((pf->goals[i].remote_agent != Arg) && (pf->goals[i].exec_state != FAILED))
	  {
	    if (show_join) { printf("JOIN %p: STARTING %d backtracable\n",Arg,i); fflush(stdout); }
	    if (show_join) { printf("JOIN %p:  remote agent %p\n",Arg,pf->goals[i].remote_agent); fflush(stdout); }
	    Wait_Acquire_slock(Mutex_Lock_Of(pf->goals[i].remote_agent));
	    if (show_join) { printf("JOIN %p: STARTING adding backtracable list\n",Arg); fflush(stdout); }
	    add_handler_to_back_goal_list(Arg,pf->goals + i);
	    if (show_join) { printf("JOIN %p: adding backtracable list DONE\n",Arg); fflush(stdout); }
	    Release_slock(Mutex_Lock_Of(pf->goals[i].remote_agent));
	    if (show_join) { printf("JOIN %p: Goal %d backtracable DONE\n",Arg,i); fflush(stdout); }
	    Wait_Acquire_lock(Waiting_For_Work_Lock_Of(pf->goals[i].remote_agent));
	    Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(pf->goals[i].remote_agent));
	    Release_lock(Waiting_For_Work_Lock_Of(pf->goals[i].remote_agent));
	    if (show_join) { printf("JOIN %p: Goal %d RELEASED\n",Arg,i); fflush(stdout); }
	  }
      if (show_all) { printf("%p Exiting join nondet II\n",Arg); fflush(stdout); }
      return FALSE;
    }

  //Updating nowComb of right
  if (show_join) { printf("JOIN %p: Prev combining %p\n",Arg,pf->goals[right].nowComb); fflush(stdout); }
  pf->goals[right].nowComb = pf->goals[right].nowComb->next;
  if (show_join) { printf("JOIN %p: Now combining %p\n",Arg,pf->goals[right].nowComb); fflush(stdout); }

  if (show_join) { printf("JOIN %p: Combining next answer of %d\n",Arg,right); fflush(stdout); }

  //Generating the answer in the heap
  if (pf->goals[right].nowComb->pHeap != NULL)
    {
      if (show_join) { printf("JOIN %p: Reusing %d numVars\n",Arg,right,(pf)->goals[right].nowComb->numVars); fflush(stdout); } 
      PutAnswerBindingsFromHeap(pf->goals[right].nowComb);
      if (show_join) { printf("JOIN %p: PutAnswerBindingsFromHeap DONE\n",Arg); fflush(stdout); }
    }
  else 
    {
      if (show_join) { printf("JOIN %p: Reusing %d numVars\n",Arg,right,(pf)->goals[right].nowComb->numVars); fflush(stdout); } 
      NowCombAnswerToHeap(pf->goals[right]);
      if (show_join) { printf("JOIN %p: NowCombAnswerToHeap DONE\n",Arg); fflush(stdout); }
      //protect the new answer
    }
  
  for (right = right + 1; right < pf->numGoals; right++)
    {
      if (pf->goals[right].combining) continue;
      
      //Updating nowComb = firstAns to the right (except combined)
      pf->goals[right].nowComb = pf->goals[right].firstAns;
      //Trailing first answer bindings
      PutAnswerBindingsFromHeap(pf->goals[right].nowComb);
      if (show_join) { printf("JOIN %p: PutAnswerBindingsFromHeap %d\n",Arg,right); fflush(stdout); }
    }
  if (show_all) { printf("%p Exiting join nondet III\n",Arg); fflush(stdout); }

  //Looking the most right goal with uncombined answers
  int comb;
  for (right = pf->numGoals - 1; (right >= 0) && (pf->goals[right].nowComb == pf->goals[right].lastComb); right--)
    {
      if (pf->goals[right].combining) comb = right;
    }
  
  if (show_join) { printf("JOIN %p: Next answer taken in %d\n",Arg,right); fflush(stdout); }
  if (show_join) { printf("JOIN %p: PF %p PRE node->trail %p\n",Arg,pf,Arg->node->trail_top); fflush(stdout); }
  Arg->node->term[1] = MakeInteger(Arg, (tagged_t)right);
  if (right >= 0) Change(Arg->node->trail_top,pf->goals[right].nowComb->trail);
  else Change(Arg->node->trail_top,pf->goals[comb].nowComb->trail);
  if (show_join) { printf("JOIN %p: POST node->trail %p\n",Arg,Arg->node->trail_top); fflush(stdout); }

  //The choice point protects the generated answers (and the right number) from backtracking
  Arg->node->global_top = Arg->global_top;
  
#endif
  return TRUE;
}

bool_t apll_parback_new_answer(Arg)
     Argdecl;
{
#if defined(PARBACK)
  if (show_all) { printf("%p Entering new_answer\n",Arg); fflush(stdout); }

  DEREF(X(0),X(0));
  par_goal_t *goal = (par_goal_t *)GetInteger(X(0));

// ESTO NO ES ASÍ
//  if (goal->exec_state == SPEC_SUSPENDED) 
//    { 
//      if (show_all) {printf("%p Suspended Goal\n",Arg); fflush(stdout); }
//      if (show_all) { printf("%p Exiting new_answer\n",Arg); fflush(stdout); }
//      return TRUE;
//    }
  
  //Memory for the new answer structure
  if (goal->firstAns==NULL) 
    {
      goal->firstAns = (AL_t*) checkalloc (sizeof(AL_t));
      goal->firstAns->next = NULL;
      goal->lastAns = goal->firstAns;
      goal->lastComb = goal->firstAns;
      Wait_Acquire_slock(goal->pf->vars_l);
      goal->pf->ansPending--;
      Release_slock(goal->pf->vars_l);
    }
  else
    {
      goal->lastAns->next = (AL_t*) checkalloc (sizeof(AL_t));
      goal->lastAns = goal->lastAns->next;
      goal->lastAns->next = NULL;
    }

  if (show_new_answer) {printf("NA %p: ansPending %d\n",Arg,goal->pf->ansPending); fflush(stdout);}
  if (show_new_answer) {printf("NA %p: combining %d\n",Arg,goal->pf->combining); fflush(stdout);}
  Wait_Acquire_slock(goal->pf->vars_l);
  if ((goal->pf->ansPending == 0) && !goal->pf->combining)
    {
      if (show_new_answer) {printf("NA %p: COMBINAMOS\n",Arg); fflush(stdout);}
      goal->pf->combining = TRUE;
      goal->lastComb = goal->lastAns;
      goal->combining = TRUE;
    }
  Release_slock(goal->pf->vars_l);
  if (show_new_answer) {printf("NA %p: %p lastAns %p \n",Arg,goal,goal->lastAns); fflush(stdout);}
  
  //goal->lastAns points to the new answer structure

  if (goal->ch_init->local_top == NULL) { printf("\nPROBLEMAS LOCAL TOP\n"); fflush(stdout); }

  //looking for answer variables
  tagged_t *iTrail;
  if (show_new_answer) {printf("NA %p: guardo desde %p a %p\n",Arg,Arg->trail_top - 1,goal->trail_init); fflush(stdout);}
  int numVars = 0;
  for (iTrail = Arg->trail_top - 1; !TrailYounger(Alig(goal->trail_init),iTrail); iTrail--)
    {
      //PENDING: o si es un puntero a agente remoto!!!
      if ((TagIsHVA(*iTrail) && (HeapYounger(goal->ch_init->global_top,TagToPointer(*iTrail)) ||
				 HeapYounger(TagToPointer(*iTrail),Heap_End))) ||
	  (TagIsSVA(*iTrail) && (StackYounger(goal->ch_init->local_top,TagToPointer(*iTrail)) ||
				 StackYounger(TagToPointer(*iTrail),Stack_End))))
	{
	  numVars++;
	}
    }

  goal->lastAns->numVars = 0;  
  if (numVars > 0) 
    {
      goal->lastAns->vars = checkalloc (numVars * sizeof(tagged_t));
      for (iTrail = Arg->trail_top - 1; !TrailYounger(Alig(goal->trail_init),iTrail); iTrail--)
	{
	  //PENDING: o si es un puntero a agente remoto!!!
	  if ((TagIsHVA(*iTrail) && (HeapYounger(goal->ch_init->global_top,TagToPointer(*iTrail)) ||
				     HeapYounger(TagToPointer(*iTrail),Heap_End))) ||
	      (TagIsSVA(*iTrail) && (StackYounger(goal->ch_init->local_top,TagToPointer(*iTrail)) ||
				     StackYounger(TagToPointer(*iTrail),Stack_End))))
	    {
	      if (show_new_answer) {printf("NA %p: guardo %p = %p -> %p\n",Arg,iTrail,*iTrail,CTagToPointer(*iTrail)); fflush(stdout);}
	      goal->lastAns->vars[goal->lastAns->numVars] = *iTrail;
	      goal->lastAns->numVars++;
	    }
	  else
	    if (show_new_answer) {printf("NA %p: NO guardo %p = %p -> %p\n",Arg,iTrail,*iTrail,CTagToPointer(*iTrail)); fflush(stdout);}
	}
    }

  //creating the answer term in the goal memory
  goal->lastAns->start = goal->free;
  make_ground(goal, goal->lastAns, Heap_End);
  goal->lastAns->end = goal->free;

  if (show_new_answer) {printf("NA %p: goal->agent %p goal->remote_agent %p goal->init %p Arg->node %p\n",Arg,
			       goal->agent, goal->remote_agent, goal->ch_init, Arg->node); fflush(stdout);}      
    
  //Freeing from the last choice point (answer has been frozen)
  if (show_new_answer) {printf("NA %p: LIBERA MEMORIA\n",Arg); fflush(stdout);}      
  Heap_Warn_Soft = Int_Heap_Warn;
  RestoreGtop(Arg->node);
  RestoreLtop(Arg->node);
  SetShadowregs(Arg->node);
  if (show_new_answer) 
    {
      printf("NA %p: NODE %p libero desde %p a %p\n",Arg,Arg->node,Arg->trail_top-1,Arg->node->trail_top); 
      fflush(stdout);
    }      
  while (TrailYounger(Arg->trail_top,Alig(Arg->node->trail_top)))
    {
      tagged_t term = TrailPop(Arg->trail_top);
      if (show_new_answer) {printf("NA %p: %p unbound\n",Arg,term); fflush(stdout);}            
      CTagToPointer(term) = term;
    }
  if (show_new_answer) {printf("NA %p: Arg->trail_top %p\n",Arg,Arg->trail_top); fflush(stdout);}      

  if (show_all) { printf("%p Exiting new_answer %p %p - NODE %p\n",Arg,goal->remote_agent,goal->agent,Arg->node); fflush(stdout); }

  //Memo the answer (no more choices -> FAIL -> deterministic!!!)
  //  Local cannot fail because forward execution would be penalized.
  //  if (((goal->agent == goal->remote_agent) && (goal->init == Arg->node)) || 
  //      ((goal->agent != goal->remote_agent) && (goal->init == ChoiceCharOffset(Arg->node,-Arg->node->next_alt->node_offset))))
  if ((goal->agent != goal->remote_agent) && (goal->ch_init == ChoiceCharOffset(Arg->node,-Arg->node->next_alt->node_offset)))
    {
      Release_slock(Mutex_Lock);
      return FALSE;
    }
#endif

  return TRUE;
}

/* Initialization procedure */
bool_t init_parback(Arg)
     Argdecl;
{
#if defined(PARBACK)
  fifo             = init_atom_check("fifo");
  lifo             = init_atom_check("lifo");
  det              = init_atom_check("det");
  address_nd_fork_c = def_retry_c(nd_fork_c,1);
  address_nd_join_c = def_retry_c(nd_join_c,2);
  show_all = FALSE;
  show_cancellation = FALSE;
  show_move_top = FALSE;
  show_fork = FALSE;
  show_join = FALSE;
  show_state = FALSE;
  show_limits = FALSE;
  show_lists = FALSE;
  show_new_answer = FALSE;
  show_mutex = FALSE;
#endif

  return TRUE;
}

