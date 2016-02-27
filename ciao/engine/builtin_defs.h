/*

 */

bool_t set_predtrace(Argdecl);
bool_t run_determ_c(Argdecl, tagged_t goal);
void wr_tagged(Argdecl, tagged_t t);
void wr_tagged_rec(Argdecl, tagged_t t);
void checkasserts(void);
void wr_functor(char *s, definition_t *func);
void wr_functor_1(definition_t *func);
void wr_call(Argdecl, char *s, definition_t *func);
void wr_functor_spec(Argdecl, tagged_t t);
void wr_x(Argdecl, int i);
void wr_y(Argdecl, int i);
