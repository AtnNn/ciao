
/*
  static LOCK_BLOCK_P new_lock_block(LOCK_BLOCK_P old_block);
 */



/*
void init_dynamic_locks(void);
LOCK create_dynamic_lock(void);
*/

#if defined(DEBUG)

#if defined(Win32)
bool_t lock_is_unset_win32(LOCK *p);
#else
bool_t lock_is_unset(LOCK *p);
#endif

unsigned long int get_inc_counter(void);
void reset_counter(void);

bool_t prolog_lock_atom(Argdecl);
bool_t prolog_unlock_atom(Argdecl);
bool_t prolog_lock_atom_state(Argdecl);

tagged_t lock_to_term(Argdecl, LOCK *l);
tagged_t slock_to_term(Argdecl, SLOCK *s);
void term_to_lock(register tagged_t t, LOCK **l);
void term_to_slock(register tagged_t t, SLOCK **s);
#endif
