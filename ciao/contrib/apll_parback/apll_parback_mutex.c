/* Attemps to enter into the mutual exclusion */
bool_t apll_parback_enter_mutex(Arg)
     Argdecl;
{
#if defined(PARBACK)
  DEREF(X(0),X(0));
  par_goal_t *h = (par_goal_t *)GetInteger(X(0));
  Safe_To_Cancel = FALSE;
  Wait_Acquire_slock(Mutex_Lock_Of(h->agent));
#endif

  return TRUE;
}

/* Attemps to enter into the local mutual exclusion */
bool_t apll_parback_enter_mutex_self(Arg)
     Argdecl;
{
#if defined(PARBACK)
  Safe_To_Cancel = FALSE;
  Wait_Acquire_slock(Mutex_Lock);
#endif

  return TRUE;
}

/* Attemps to enter into the remote mutual exclusion */
bool_t apll_parback_enter_mutex_remote(Arg)
     Argdecl;
{
#if defined(PARBACK)
  DEREF(X(0),X(0));
  par_goal_t *h = (par_goal_t *)GetInteger(X(0));
  Safe_To_Cancel = FALSE;
  Wait_Acquire_slock(Mutex_Lock_Of(h->remote_agent));
#endif

  return TRUE;
}

/* Exits from the mutual exclusion */
bool_t apll_parback_exit_mutex(Arg)
     Argdecl;
{
#if defined(PARBACK)
  DEREF(X(0),X(0));
  par_goal_t *h = (par_goal_t *)GetInteger(X(0));
  Release_slock(Mutex_Lock_Of(h->agent));
  Safe_To_Cancel = TRUE;
#endif

  return TRUE;
}

/* Exits from the local mutual exclusion */
bool_t apll_parback_exit_mutex_self(Arg)
     Argdecl;
{
#if defined(PARBACK)
  Release_slock(Mutex_Lock);
  Safe_To_Cancel = TRUE;
#endif

  return TRUE;
}

/* Exits from the remote mutual exclusion */
bool_t apll_parback_exit_mutex_remote(Arg)
     Argdecl;
{
#if defined(PARBACK)
  DEREF(X(0),X(0));
  par_goal_t *h = (par_goal_t *)GetInteger(X(0));
  Release_slock(Mutex_Lock_Of(h->remote_agent));
  Safe_To_Cancel = TRUE;
#endif

  return TRUE;
}

/* Suspends the execution of the current thread */
bool_t apll_parback_suspend(Arg)
     Argdecl;
{
#if defined(PARBACK)

  if (show_mutex) {printf("%p to SUSPEND\n",Arg); fflush(stdout);}      
//  Wait_Acquire_lock(Waiting_For_Work_Lock);
//  Release_slock(Mutex_Lock_Of(h->remote_agent));
  Suspended_Waiting_For_Work = TRUE;
  Cond_Var_Wait(Waiting_For_Work_Cond_Var,Waiting_For_Work_Lock);
  
  Suspend = RELEASED;
  Suspended_Waiting_For_Work = FALSE;
  Release_lock(Waiting_For_Work_Lock);
  if (show_mutex) {printf("%p from SUSPEND\n",Arg); fflush(stdout);}      
#endif

  return TRUE;
}


/* Releases the execution of the publishing agent */
bool_t apll_parback_release(Arg)
     Argdecl;
{
#if defined(PARBACK)
  DEREF(X(0),X(0));
  par_goal_t *h = (par_goal_t *)GetInteger(X(0));
  Wait_Acquire_lock(Waiting_For_Work_Lock_Of(h->agent));
  Cond_Var_Broadcast(Waiting_For_Work_Cond_Var_Of(h->agent));
  Release_lock(Waiting_For_Work_Lock_Of(h->agent));
#endif

  return TRUE;
}
