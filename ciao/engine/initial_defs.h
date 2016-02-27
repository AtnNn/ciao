/*
  static void classify_atom(s);
  static bool_t prolog_ciao_lib_dir(Argdecl);
  static bool_t prolog_ciao_c_headers_dir(Argdecl);
  static void initialize_intrinsics(void);
  static void deffunction(char *atom, CInfo proc, int arity, inf funcno);
  static void define_functions(void);
  static bool_t prolog_atom_mode(Argdecl);
  static definition_t *define_builtin(char *pname, int instr, int arity, int public);

 */

#if defined(USE_ATOM_LEN)
atom_t *new_atom_check(unsigned char *str, 
                            unsigned int str_len,
                            unsigned int index);
#else
atom_t *new_atom_check(unsigned char *str, 
                            unsigned int index);
#endif

definition_t *define_c_predicate(char *pname, 
				 bool_t (*procedure)(), 
				 int arity);
void glb_init_each_time(void);
void init_each_time(Argdecl);
void init_kanji(void);
void init_latin1(void);
#if defined(USE_OWN_MALLOC)
void init_own_malloc(void);
#endif
void init_once(void);
void init_locks(void);
void init_streams(void);
void init_streams_each_time(Argdecl);
void local_init_each_time(Argdecl);
/*void reclassify_atoms(void);*/
void reinitialize(Argdecl);
