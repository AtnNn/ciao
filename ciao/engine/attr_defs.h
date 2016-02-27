bool_t bu1_detach_attribute(Argdecl, register tagged_t x);
bool_t bu2_attach_attribute(Argdecl, register tagged_t var, register tagged_t constr);
bool_t bu2_update_attribute(Argdecl, register tagged_t x, register tagged_t constr);
tagged_t fu1_get_attribute(Argdecl, register tagged_t x);
tagged_t fu1_type(Argdecl, register tagged_t t0);
void collect_one_pending_unification(Argdecl);
void collect_pending_unifications(Argdecl,int wake_count);
