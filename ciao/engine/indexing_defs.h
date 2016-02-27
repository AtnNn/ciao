/*
  static void set_nondet(register try_node_t *t, incore_info_t *def, bool_t first);
  static void incore_insert(register try_node_t **t0, int effar, emul_info_t *ref, incore_info_t *def);
  static try_node_t *incore_copy(try_node_t *from);
  static void incore_puthash(sw_on_key_t **psw, int effar, emul_info_t *current, incore_info_t *def, tagged_t k);
  static void free_try(try_node_t **t);
  static void free_sw_on_key(sw_on_key_t **sw);
  static void free_emulinfo(register emul_info_t *cl);
  static void free_incoreinfo(register incore_info_t **p);
  static void make_undefined(Argdecl, definition_t *f);
  static void free_info(int insn, char *info);
  static void init_interpreted(register definition_t *f);
 */

sw_on_key_node_t *incore_gethash(register sw_on_key_t *sw, tagged_t key);
sw_on_key_t *new_switch_on_key(int size, try_node_t *otherwise);
void leave_to_gc(int type, char *info);
bool_t empty_gcdef_bin(Argdecl);
void relocate_gcdef_clocks(instance_clock_t *clocks);
bool_t prolog_abolish(Argdecl);                                       /* JFMC */
bool_t abolish(Argdecl, CIAO_REGISTER definition_t *f);               /* JFMC */
bool_t define_predicate(Argdecl);
bool_t erase_clause(Argdecl);
bool_t clause_number(Argdecl);
bool_t compiled_clause(Argdecl);
sw_on_key_node_t *dyn_puthash(sw_on_key_t **swp, tagged_t k);
bool_t set_property(Argdecl);
