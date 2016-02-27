
/*
  static LOCK_BLOCK_P new_lock_block(LOCK_BLOCK_P old_block);
 */

BOOL lock_is_unset(LOCK p);
void init_dynamic_locks(void);
LOCK create_dynamic_lock(void);

#if defined(DEBUG)
unsigned long int get_inc_counter(void);
void reset_counter(void);
#endif
