/* Saves a pointer to the initial choice point of the goal execution
   in the handler */
bool_t apll_parback_save_init_execution(Arg)
     Argdecl;
{
#if defined(PARBACK)
  if (show_all) { printf("%p Entering apll_parback_save_init_execution\n",Arg); fflush(stdout); }
  DEREF(X(0),X(0));
  par_goal_t *h = (par_goal_t *)GetInteger(X(0));

  h->ch_init = Arg->node;
  h->trail_init = Arg->trail_top;
  if (show_limits) { printf("%p SAVE_INIT %p: node %p trail %p\n",Arg,h,Arg->node,Arg->node->trail_top); fflush(stdout); }

  if (show_all) { printf("%p Exiting apll_parback_save_init_execution\n",Arg); fflush(stdout); }
#endif

  return TRUE;
}

/* Saves a pointer to the final choice point of the goal execution in
   the handler */
bool_t apll_parback_save_end_execution(Arg)
     Argdecl;
{
#if defined(PARBACK)
  if (show_all) { printf("%p Entering apll_parback_save_end_execution\n",Arg); fflush(stdout); }
  DEREF(X(0),X(0));
  par_goal_t *h = (par_goal_t *)GetInteger(X(0));

  h->ch_end = Arg->node;
  h->trail_end = Arg->trail_top;
  if (show_limits) { printf("%p SAVE_END %p: node %p trail %p\n",Arg,h,Arg->node,Arg->node->trail_top); fflush(stdout); }

  if (show_all) { printf("%p Exiting apll_parback_save_end_execution\n",Arg); fflush(stdout); }
#endif

  return TRUE;
}

/* Returns the goal associated to the handler */
bool_t apll_parback_retrieve_goal(Arg)
     Argdecl;
{
#if defined(PARBACK)
  if (show_all) { printf("%p Entering apll_parback_retrieve_goal\n",Arg); fflush(stdout); }
  DEREF(X(0),X(0));
  par_goal_t *h = (par_goal_t *)GetInteger(X(0));

  Unify(X(1), h->goal);
  if (show_all) { printf("%p Exiting apll_parback_retrieve_goal\n",Arg); fflush(stdout); }
#endif

  return TRUE;
}
