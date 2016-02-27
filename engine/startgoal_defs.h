void firstgoal(wrb_state_p firstworker, char *goal_name);
void *startgoal_cont(void *goal_and_conts);
void *startgoal_copyvar(void *goal);
void *startgoal_shvar(void *goal);
void *startgoal(void *goal);
void *startgoal_simp(void *goal);
wrb_state_p gimme_a_new_worker(worker_entry_p worker_entry);
