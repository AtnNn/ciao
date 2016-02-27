/*

  static void relocate_table_clocks(sw_on_key_t *sw, instance_clock_t *clocks)

*/

bool_t prolog_purge(Argdecl);
bool_t prolog_erase(Argdecl);
bool_t prolog_ptr_ref(Argdecl);
bool_t inserta(Argdecl);
bool_t insertz(Argdecl);
int compile_large(tagged_t t, insn_t *p);
bool_t make_bytecode_object(Argdecl);
instance_t *active_instance(Argdecl,  instance_t *i, int itime, bool_t normal);
void clock_overflow(Argdecl);
void relocate_clocks( instance_t *inst,  instance_clock_t *clocks);
void expunge_instance( instance_t *i);
instance_t *active_instance_conc(Argdecl,  instance_t *i, int_info_t *pred_root);
