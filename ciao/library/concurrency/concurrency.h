#if defined(THREADS) && defined(USE_POSIX_THREADS)
extern pthread_attr_t detached_thread;
extern pthread_attr_t joinable_thread;
#endif

extern bool_t    prolog_eng_kill(Argdecl);
extern bool_t    prolog_eng_killothers(Argdecl);
extern bool_t    prolog_eng_wait(Argdecl);
extern bool_t    prolog_eng_self(Argdecl);
extern bool_t    prolog_eng_status(Argdecl);
extern bool_t    prolog_eng_status1(Argdecl);
extern bool_t    prolog_eng_backtrack(Argdecl);
extern bool_t    prolog_eng_release(Argdecl);
extern bool_t    prolog_eng_cut(Argdecl);
extern ENG_INT goal_from_thread_id(THREAD_ID id);


