/*
  static int compare_aux(Argdecl, tagged_t x1, tagged_t x2)
static int compare_args_aux(Argdecl, register int arity, register tagged_t *pt1, register tagged_t *pt2, tagged_t *x1, tagged_t *x2);
 */



int compare_help(Argdecl, tagged_t x1, tagged_t x2);
bool_t prompt(Argdecl);
bool_t unknown(Argdecl);
bool_t metachoice(Argdecl);
bool_t metacut(Argdecl);
bool_t retry_cut(Argdecl);
bool_t setarg(Argdecl);
bool_t undo(Argdecl);
bool_t frozen(Argdecl);
bool_t defrost(Argdecl);
bool_t debugger_state(Argdecl);
bool_t debugger_mode(Argdecl);
bool_t leash_mode(Argdecl);
bool_t maxdepth(Argdecl);
bool_t printdepth(Argdecl);
bool_t breaklevel(Argdecl);
bool_t compiling(Argdecl);
bool_t ferror_flag(Argdecl);
bool_t single_var_flag(Argdecl);
bool_t character_escapes_flag(Argdecl);
bool_t redefine_flag(Argdecl);
bool_t quiet_flag(Argdecl);
bool_t spypoint(Argdecl);
bool_t prolog_radix(Argdecl);
bool_t constraint_list(Argdecl);
int find_constraints(Argdecl, tagged_t *limit);
bool_t prolog_eq(Argdecl);
bool_t prolog_dif(Argdecl, definition_t *address_dif);
bool_t large_data(Argdecl);
bool_t prolog_interpreted_clause(Argdecl);
bool_t prolog_address(Argdecl);
int_info_t *current_clauses_aux(tagged_t head);
bool_t insertz_aux(int_info_t *root, instance_t *n);
int var_address(Argdecl, tagged_t term);
