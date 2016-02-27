/*
  static void add_symbol_table(void *handle, char *path);
  static int write_glue_functions(FILE *f, TAGGED flist, TAGGED dlist);
  static int list_to_ext_file(TAGGED flist, TAGGED dlist, char *buffer, char *pathname);
  static char *list_to_string(TAGGED list, char *buffer, char *prefix);
  static int activate_code(rawaddr,rawsize);
  static BOOL internal_load_foreign_files(Argdecl);
  static BOOL internal_load_foreign_files(Arg);
 */



char *address_align(char *cp);
int reactivate_code(void);

BOOL foreign_ci_inarg(Argdecl, int farg, int spec);
BOOL foreign_ci_outarg(Argdecl, int farg, int spec);
BOOL foreign_ci_retval(Argdecl, int farg, int spec);
char *ENG_string_from_atom(ENG_INT atom);
TAGGED ENG_atom_from_string(char *string);
unsigned long ENG_padded_string_from_atom(long unsigned int *atom, register char *string, long unsigned int *length);
unsigned long ENG_atom_from_padded_string(long unsigned int *atom, char *string, long unsigned int *length);
BOOL prolog_load_foreign_files(Argdecl);
BOOL prolog_prepare_foreign_files(Argdecl);
BOOL prolog_foreign_base(Argdecl);
