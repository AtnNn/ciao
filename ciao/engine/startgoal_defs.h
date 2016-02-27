void firstgoal(goal_descriptor_t *firstworker, char *goal_name);
THREAD_RES_T startgoal(THREAD_ARG wo);
THREAD_RES_T make_backtracking(THREAD_ARG wo);

extern bool_t (*eng_killothers_startgoal)(Argdecl);
extern bool_t (*eng_killothers_ciao_prolog)(Argdecl);


