/*
  static void writechar(int ch, register int i, register struct stream_node *s);
  static int readchar(register struct stream_node *s, int valpat, int typpat, struct definition *pred_address);
static void display_term(Argdecl, TAGGED term, struct stream_node *stream, BOOL quoted);
 */

BOOL flush_output(Argdecl);
BOOL flush_output1(Argdecl);
BOOL getct(Argdecl);
BOOL getct1(Argdecl);
BOOL get(Argdecl);
BOOL get2(Argdecl);
BOOL get1(Argdecl);
BOOL get12(Argdecl);
BOOL nl(Argdecl);
BOOL nl1(Argdecl);
BOOL put(Argdecl);
BOOL put2(Argdecl);
BOOL tab(Argdecl);
BOOL tab2(Argdecl);
BOOL skip(Argdecl);
BOOL skip2(Argdecl);
void print_string(register struct stream_node *stream, register char *p);
void print_variable(Argdecl, struct stream_node *stream, TAGGED term);
void print_number(Argdecl, struct stream_node *stream, TAGGED term);
void print_atom(struct stream_node *stream, TAGGED term);
BOOL prolog_display(Argdecl);
BOOL prolog_display2(Argdecl);
BOOL prolog_displayq(Argdecl);
BOOL prolog_displayq2(Argdecl);
BOOL prolog_clearerr(Argdecl);
