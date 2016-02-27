
#if defined(THREADS) && defined(USE_POSIX_THREADS)
extern pthread_attr_t detached_thread;
extern pthread_attr_t joinable_thread;
#endif

BOOL prolog_launch_goal(register struct worker *w);
BOOL prolog_launch_goal_2(register struct worker *w);
BOOL prolog_kill_thread(register struct worker *w);
BOOL prolog_kill_other_threads(register struct worker *w);
BOOL prolog_tasks_status(register struct worker *w);
BOOL prolog_lock_term(register struct worker *w);
BOOL prolog_unlock_term(register struct worker *w);
void make_worker_entry_free(worker_entry_p we);
wrb_state_p init_first_worker_entry(void);
void init_worker_entry_table(void);
