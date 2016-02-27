

int kill_thread(wrb_state_p worker_to_kill);
void kill_other_threads(void);
wrb_state_p look_for_a_free_worker(void);
struct worker *get_my_worker(void);
void allow_thread_cancellation(void);
wrb_state_p attach_me_to_wrb_state_list(Argdecl);
void disallow_thread_cancellation(void);
void init_wrb_state_list(void);
void make_worker_free(wrb_state_p);
void print_task_status(Argdecl);
