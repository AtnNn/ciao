
/*
  static int ql_getc(qlstream_t *s);
  static int qlgetshort(qlstream_t *f);
  static ENG_INT qlgetlong(qlstream_t *f);
  static char *qlgetstring(FILE *f);
  static tagged_t qlgetlarge(register worker_t *w, qlstream_t *f);
  static ENG_FLT qlgetdouble(qlstream_t *f);
  static void ql_load_dbnode(register worker_t *w, int Li, qlstream_t *f, int codelength, int counter_cnt);
 */


void qlgetbytecode(register worker_t *w, qlstream_t *f, bcp_t insn_p, int length);
bool_t qlinsert(register worker_t *w, qlstream_t *qlcode, tagged_t *rungoal);
