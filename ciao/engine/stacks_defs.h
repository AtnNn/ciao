/*

*/

void choice_overflow(Argdecl, int pad);
void stack_overflow(Argdecl);
bool_t gc_start(Argdecl);
void heap_overflow(Argdecl, int pad);
void collect_goals_from_trail(Argdecl, int wake_count);
void trail_gc(Argdecl);
bool_t stack_shift_usage(Argdecl);
bool_t termheap_usage(Argdecl);
bool_t envstack_usage(Argdecl);
bool_t choice_usage(Argdecl);
bool_t trail_usage(Argdecl);
void explicit_heap_overflow(Argdecl, int pad, int arity);

#if defined(ANDPARALLEL)
bool_t is_rem_Hterm(CIAO_REGISTER tagged_t term, worker_t *w, worker_t *remote_w);
void heap_overflow_adjust_wam(worker_t *w, int reloc_factor, tagged_t *newh, bool_t remote_reloc, worker_t *remote_worker);
#else
void heap_overflow_adjust_wam(worker_t *w, int reloc_factor, tagged_t *newh);
#endif
void stack_overflow_adjust_wam(worker_t *w, int reloc_factor);


