#if defined(THREADS) && defined(USE_POSIX_THREADS)
extern pthread_attr_t joinable_thread;
#endif

/* state of execution */
#define NOTEXECUTED    0
#define REM_EXECUTING  1
#define REEXECUTE      2
#define CANCELLING     3
#define CANCELLED      4
#define FINISHED       5
#define NO_MORE_SOLS   6
#define MORE_SOLS      7

#define NUMBER_STATES  8


/* ****************************************** */
/*   ANDPROLOG LOW-LEVEL SUPPORT PRIMITIVES   */
/* ****************************************** */
extern bool_t apll_start_thread(Argdecl);
extern bool_t apll_number_agents(Argdecl);
extern bool_t apll_push_goal(Argdecl);
extern bool_t apll_find_goal(Argdecl);
extern bool_t apll_goal_available(Argdecl);
extern bool_t apll_cancellation(Argdecl);
extern bool_t apll_free_memory(Argdecl);
extern bool_t apll_retrieve_goal(Argdecl);
extern bool_t apll_goal_det(Argdecl);
extern bool_t apll_set_goal_det(Argdecl);
extern bool_t apll_set_goal_nondet(Argdecl);
extern bool_t apll_goal_not_executed(Argdecl);
extern bool_t apll_set_goal_not_executed(Argdecl);
extern bool_t apll_goal_rem_executing(Argdecl);
extern bool_t apll_set_goal_rem_executing(Argdecl);
extern bool_t apll_goal_finished(Argdecl);
extern bool_t apll_set_goal_finished(Argdecl);
extern bool_t apll_goal_tobacktrack(Argdecl);
extern bool_t apll_set_goal_tobacktrack(Argdecl);
extern bool_t apll_goal_toreexecute(Argdecl);
extern bool_t apll_set_goal_toreexecute(Argdecl);
extern bool_t apll_goal_failed(Argdecl);
extern bool_t apll_set_goal_failed(Argdecl);
extern bool_t apll_goal_cancelled(Argdecl);
extern bool_t apll_set_goal_cancelled(Argdecl);

extern bool_t apll_send_event(Argdecl);
extern bool_t apll_read_event(Argdecl);
extern bool_t apll_save_init_execution(Argdecl);
extern bool_t apll_save_end_execution(Argdecl);
extern bool_t apll_more_solutions(Argdecl);
extern bool_t apll_move_execution_top(Argdecl);
extern bool_t apll_move_pointers_down(Argdecl);
extern bool_t apll_metacut_garbage_slots(Argdecl);

extern bool_t apll_waiting(Argdecl);
extern bool_t apll_suspend(Argdecl);
extern bool_t apll_release(Argdecl);
extern bool_t apll_release_remote(Argdecl);
extern bool_t apll_release_some_suspended_thread(Argdecl);
extern bool_t apll_release_all_for_unwinding(Argdecl);
extern bool_t apll_enter_mutex(Argdecl);
extern bool_t apll_enter_mutex_self(Argdecl);
extern bool_t apll_enter_mutex_remote(Argdecl);
extern bool_t apll_exit_mutex(Argdecl);
extern bool_t apll_exit_mutex_self(Argdecl);
extern bool_t apll_exit_mutex_remote(Argdecl);

extern bool_t apll_clean_measures(Argdecl);
extern bool_t apll_print_measures(Argdecl);
extern bool_t apll_new_measure(Argdecl);
extern bool_t apll_not_measure(Argdecl);
extern bool_t apll_incr_num_local_backtr(Argdecl);

extern bool_t init(Argdecl);

