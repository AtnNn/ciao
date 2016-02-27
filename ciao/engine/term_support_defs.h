
bool_t compile_term(Argdecl, worker_t **new_worker);
bool_t prolog_atom_codes(Argdecl);
bool_t prolog_atom_length(Argdecl);
bool_t prolog_sub_atom(Argdecl);
bool_t prolog_atom_concat(Argdecl);
bool_t prolog_copy_term(Argdecl);
bool_t prolog_cyclic_term(Argdecl);
bool_t c_cyclic_term(Argdecl, tagged_t);
bool_t prolog_unifiable(Argdecl);
tagged_t cross_copy_term(Argdecl, tagged_t remote_term);
bool_t prolog_init_radix(void);
bool_t prolog_name(Argdecl);
bool_t prolog_number_codes_2(Argdecl);
bool_t prolog_number_codes_3(Argdecl);
instance_t *compile_term_aux(Argdecl, 
                                  tagged_t head, 
                                  tagged_t body, 
                                  worker_t **new_worker);
void number_to_string(Argdecl, tagged_t term, int base);
bool_t string_to_number(Argdecl, char *AtBuf, int base, tagged_t *strnum, int arity);
