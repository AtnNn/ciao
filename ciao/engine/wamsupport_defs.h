
tagged_t evaluate(Argdecl, register tagged_t v);
int p2_offset(int insn);
//try_node_t *def_retry_c(bool_t (*proc) (worker_t *w), int arity);
try_node_t *def_retry_c(bool_t (*proc) (), int arity);
void init_startgoalcode(bcp_t b);
void init_bootcode(bcp_t b);
#if defined(INTERNAL_CALLING)
void init_internal_calling(bcp_t b);
#endif
void init_startgoalcode_cont(bcp_t b);
void init_contcode(bcp_t b, bcp_t f);
