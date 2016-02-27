/* Returns whether the goal execution has finished or not */
bool_t apll_parback_goal_not_executing(Arg)
     Argdecl;
{
#if defined(PARBACK)
  if (show_all) { printf("%p Entering apll_parback_goal_not_executing\n",Arg); fflush(stdout); }
  DEREF(X(0),X(0));
  par_goal_t *h = (par_goal_t *)GetInteger(X(0));
  Wait_Acquire_slock(h->pf->vars_l);
  if (((h->exec_state == ANSWER_FOUND) && (h->pf->combining)) || (h->exec_state == FAILED))
    {
      if (show_all) { printf("%p Exiting apll_parback_goal_not_executing I\n",Arg); fflush(stdout); }
      if (show_all) { printf("state %d combining %d\n",Arg,h->exec_state,h->pf->combining); fflush(stdout); }
      Release_slock(h->pf->vars_l);
      return TRUE;
    }
  Release_slock(h->pf->vars_l);
  Wait_Acquire_lock(Waiting_For_Work_Lock);
  Release_slock(Mutex_Lock_Of(h->remote_agent));
  if (show_all) { printf("%p Exiting apll_parback_goal_not_executing II\n",Arg); fflush(stdout); }
  return FALSE;
#endif

  return TRUE;
}

/* Sets the goal execution to finished */
bool_t apll_parback_set_goal_answer_found(Arg)
     Argdecl;
{
#if defined(PARBACK)
  if (show_all) { printf("%p Entering apll_parback_set_goal_answer_found\n",Arg); fflush(stdout); }
  DEREF(X(0),X(0));
  par_goal_t *h = (par_goal_t *)GetInteger(X(0));
  h->exec_state = ANSWER_FOUND;
  if (show_all) { printf("%p Exiting apll_parback_set_goal_answer_found\n",Arg); fflush(stdout); }
#endif

  return TRUE;
}

/* Sets the goal execution to executing */
bool_t apll_parback_set_goal_executing(Arg)
     Argdecl;
{
#if defined(PARBACK)
  if (show_all) { printf("%p Entering apll_parback_set_goal_executing\n",Arg); fflush(stdout); }
  DEREF(X(0),X(0));
  par_goal_t *h = (par_goal_t *)GetInteger(X(0));

  h->exec_state = REM_EXECUTING;
  if (show_all) { printf("%p Exiting apll_parback_set_goal_executing\n",Arg); fflush(stdout); }
#endif

  return TRUE;
}

/* Returns whether the goal has been stolen to be executed */
bool_t apll_parback_goal_not_executed(Arg)
     Argdecl;
{
#if defined(PARBACK)
  if (show_all) { printf("%p Entering apll_parback_goal_not_executed\n",Arg); fflush(stdout); }
  DEREF(X(0),X(0));
  par_goal_t *h = (par_goal_t *)GetInteger(X(0));
  if (show_all) { printf("%p Exiting apll_parback_goal_not_executed %d\n",Arg,h->exec_state); fflush(stdout); }
  return (h->exec_state == NOT_STARTED);
#endif

  return TRUE;
}

/* Sets the goal execution to finished */
bool_t apll_parback_set_goal_failed(Arg)
     Argdecl;
{
#if defined(PARBACK)
  if (show_all) { printf("%p Entering apll_parback_set_goal_failed\n",Arg); fflush(stdout); }
  DEREF(X(0),X(0));
  par_goal_t *h = (par_goal_t *)GetInteger(X(0));
  Wait_Acquire_slock(h->pf->vars_l);
  h->pf->failPending--;
  Release_slock(h->pf->vars_l);
  h->exec_state = FAILED;
  if (show_all) { printf("%p Exiting apll_parback_set_goal_failed\n",Arg); fflush(stdout); }
#endif

  return TRUE;
}

/* Sets the goal execution to finished */
bool_t apll_parback_set_no_more_backtracking(Arg)
     Argdecl;
{
#if defined(PARBACK)
  if (show_all) { printf("%p Entering apll_parback_set_no_more_backtracking\n",Arg); fflush(stdout); }
  DEREF(X(0),X(0));
  par_goal_t *h = (par_goal_t *)GetInteger(X(0));
  if (h->list == BACK) remove_handler_from_back_goal_list(Arg,h);
  if (show_all) { printf("%p Exiting apll_parback_set_no_more_backtracking\n",Arg); fflush(stdout); }
#endif

  return TRUE;
}

