
BOOL compile_term(Argdecl, struct worker **new_worker);
BOOL prolog_atom_codes(Argdecl);
BOOL prolog_atom_length(Argdecl);
BOOL prolog_sub_atom(Argdecl);
BOOL prolog_atom_concat(Argdecl);
BOOL prolog_copy_term(Argdecl);
TAGGED cross_copy_term(Argdecl, TAGGED remote_term);
BOOL prolog_init_radix(void);
BOOL prolog_name(Argdecl);
BOOL prolog_number_codes(Argdecl);
struct instance *compile_term_aux(Argdecl, 
                                  TAGGED head, 
                                  TAGGED body, 
                                  struct worker **new_worker);
void number_to_string(Argdecl, TAGGED term, int base);
