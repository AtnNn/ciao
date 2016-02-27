/*
  static unsigned int predicate_property_bits(register definition_t *d);
 */


typedef enum {X5, X2} WhichChain;

#if defined(TABLING)
bool_t nd_fake_choicept(Argdecl);
#endif
void pop_frame(Argdecl);
void push_frame(Argdecl, int arity);
void pop_choicept(Argdecl);
void push_choicept(Argdecl, try_node_t *alt);
bool_t nd_atom_concat(Argdecl);
bool_t current_atom(Argdecl);
bool_t nd_current_atom(Argdecl);
bool_t current_clauses(Argdecl);
bool_t prolog_repeat(Argdecl);
bool_t nd_repeat(Argdecl);
bool_t current_predicate(Argdecl);
bool_t nd_current_predicate(Argdecl);
bool_t predicate_property(Argdecl);
bool_t nd_predicate_property(Argdecl);
instance_t *current_instance(Argdecl);
instance_t *current_instance_nolog(Argdecl);
bool_t first_instance(Argdecl);
bool_t close_predicate(Argdecl);
bool_t open_predicate(Argdecl);
bool_t next_instance(Argdecl, instance_t **ipp);
bool_t next_instance_conc(Argdecl, instance_t **ipp);
void move_queue(instance_handle_t **srcq, 
                instance_handle_t **destq,
                instance_t *destinst);
void jump_to_next_instance(instance_t *x2_p_insp,
                           instance_t *x5_p_insp,
                           instance_t **ipp,
                           instance_t **x2,
                           instance_t **x5);
void remove_link_chains(node_t **topdynamic,
                        node_t  *chpttoclear);

