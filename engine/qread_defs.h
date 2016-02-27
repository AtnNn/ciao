
BOOL pop_qlinfo(Argdecl);
BOOL prolog_qread(Argdecl);
BOOL push_qlinfo(Argdecl);
BOOL qread1(Argdecl, FILE *qfile, TAGGED *rungoal);
int is_a_script(FILE *file);
void expand_qload(void);
void reloc_counter(ENG_INT Label);
void reloc_emul_entry(int Li, ENG_INT Label);
void reloc_pointer(int Li, ENG_INT Label);
void skip_to_ctrl_l(FILE *file);
