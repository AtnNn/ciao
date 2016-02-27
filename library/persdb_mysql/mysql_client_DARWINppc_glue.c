#include "/home/clip/Systems/ciao/include/DARWINppc/datadefs.h"
#include "/home/clip/Systems/ciao/include/DARWINppc/support.h"
#include "/home/clip/Systems/ciao/lib/foreign_interface/foreign_interface.h"

void *mysql_init(long);
BOOL prolog_init(struct worker *w) {
  long v0;
  void *v1;
  TAGGED p1;
  DEREF(X(0), X(0));
  GET_INTEGER(0, v0);
  v1 = mysql_init(v0);
  p1 = MakeAddress(Arg, v1);
  DEREF(X(1), X(1));
  if (!cunify(Arg, X(1), p1)) return FALSE;
  return TRUE;
}

void *mysql_real_connect(void *, char *, char *, char *, char *, long, void *, long);
BOOL prolog_connect(struct worker *w) {
  void *v0;
  char *v1;
  char *v2;
  char *v3;
  char *v4;
  long v5;
  void *v6;
  long v7;
  void *v8;
  TAGGED p8;
  DEREF(X(0), X(0));
  GET_ADDRESS(0, v0);
  DEREF(X(0), X(0));
  GET_INTEGER(5, v5);
  DEREF(X(0), X(0));
  GET_ADDRESS(6, v6);
  DEREF(X(0), X(0));
  GET_INTEGER(7, v7);
  DEREF(X(0), X(0));
  GET_CSTRING_FROM_ATOM(1, v1);
  DEREF(X(0), X(0));
  GET_CSTRING_FROM_ATOM(2, v2);
  DEREF(X(0), X(0));
  GET_CSTRING_FROM_ATOM(3, v3);
  DEREF(X(0), X(0));
  GET_CSTRING_FROM_ATOM(4, v4);
  v8 = mysql_real_connect(v0, v1, v2, v3, v4, v5, v6, v7);
  p8 = MakeAddress(Arg, v8);
  FREE(v1);
  FREE(v2);
  FREE(v3);
  FREE(v4);
  DEREF(X(8), X(8));
  if (!cunify(Arg, X(8), p8)) return FALSE;
  return TRUE;
}

void mysql_close(void *);
BOOL prolog_disconnect(struct worker *w) {
  void *v0;
  DEREF(X(0), X(0));
  GET_ADDRESS(0, v0);
  mysql_close(v0);
  return TRUE;
}

long num_rows(void *);
BOOL prolog_num_rows(struct worker *w) {
  void *v0;
  long v1;
  TAGGED p1;
  DEREF(X(0), X(0));
  GET_ADDRESS(0, v0);
  v1 = num_rows(v0);
  p1 = MakeInteger(Arg, v1);
  DEREF(X(1), X(1));
  if (!cunify(Arg, X(1), p1)) return FALSE;
  return TRUE;
}

long num_fields(void *);
BOOL prolog_num_fields(struct worker *w) {
  void *v0;
  long v1;
  TAGGED p1;
  DEREF(X(0), X(0));
  GET_ADDRESS(0, v0);
  v1 = num_fields(v0);
  p1 = MakeInteger(Arg, v1);
  DEREF(X(1), X(1));
  if (!cunify(Arg, X(1), p1)) return FALSE;
  return TRUE;
}

void mysql_free_result(void *);
BOOL prolog_free_result(struct worker *w) {
  void *v0;
  DEREF(X(0), X(0));
  GET_ADDRESS(0, v0);
  mysql_free_result(v0);
  return TRUE;
}

long mysql_query(void *, char *);
BOOL prolog_query(struct worker *w) {
  void *v0;
  char *v1;
  long v2;
  TAGGED p2;
  DEREF(X(0), X(0));
  GET_ADDRESS(0, v0);
  DEREF(X(0), X(0));
  STRING_TEST(1, v1);
  DEREF(X(0), X(0));
  GET_CSTRING_FROM_LIST(1, v1);
  v2 = mysql_query(v0, v1);
  p2 = MakeInteger(Arg, v2);
  FREE(v1);
  DEREF(X(2), X(2));
  if (!cunify(Arg, X(2), p2)) return FALSE;
  return TRUE;
}

void *mysql_use_result(void *);
BOOL prolog_use_result(struct worker *w) {
  void *v0;
  void *v1;
  TAGGED p1;
  DEREF(X(0), X(0));
  GET_ADDRESS(0, v0);
  v1 = mysql_use_result(v0);
  p1 = MakeAddress(Arg, v1);
  DEREF(X(1), X(1));
  if (!cunify(Arg, X(1), p1)) return FALSE;
  return TRUE;
}

void *mysql_fetch_row(void *);
BOOL prolog_fetch_row(struct worker *w) {
  void *v0;
  void *v1;
  TAGGED p1;
  DEREF(X(0), X(0));
  GET_ADDRESS(0, v0);
  v1 = mysql_fetch_row(v0);
  p1 = MakeAddress(Arg, v1);
  DEREF(X(1), X(1));
  if (!cunify(Arg, X(1), p1)) return FALSE;
  return TRUE;
}

char *mysql_error(void *);
BOOL prolog_error_string(struct worker *w) {
  void *v0;
  char *v1;
  TAGGED p1;
  DEREF(X(0), X(0));
  GET_ADDRESS(0, v0);
  v1 = mysql_error(v0);
  p1 = MAKE_ATOM(Arg, v1);
  DEREF(X(1), X(1));
  if (!cunify(Arg, X(1), p1)) return FALSE;
  return TRUE;
}

char *nth_string(long, void *);
BOOL prolog_nth_string(struct worker *w) {
  long v0;
  void *v1;
  char *v2;
  TAGGED p2;
  DEREF(X(0), X(0));
  GET_INTEGER(0, v0);
  DEREF(X(0), X(0));
  GET_ADDRESS(1, v1);
  v2 = nth_string(v0, v1);
  p2 = MAKE_STRING(Arg, v2);
  DEREF(X(2), X(2));
  if (!cunify(Arg, X(2), p2)) return FALSE;
  return TRUE;
}

void mysql_client_init(char *module) {
  FUNCTOR_DEFINITION_CODE;
  define_c_mod_predicate(module, "init", prolog_init, 2);
  define_c_mod_predicate(module, "connect", prolog_connect, 9);
  define_c_mod_predicate(module, "disconnect", prolog_disconnect, 1);
  define_c_mod_predicate(module, "num_rows", prolog_num_rows, 2);
  define_c_mod_predicate(module, "num_fields", prolog_num_fields, 2);
  define_c_mod_predicate(module, "free_result", prolog_free_result, 1);
  define_c_mod_predicate(module, "query", prolog_query, 3);
  define_c_mod_predicate(module, "use_result", prolog_use_result, 2);
  define_c_mod_predicate(module, "fetch_row", prolog_fetch_row, 2);
  define_c_mod_predicate(module, "error_string", prolog_error_string, 2);
  define_c_mod_predicate(module, "nth_string", prolog_nth_string, 3);
}

void mysql_client_end(char *module) {
  undefine_c_mod_predicate(module, "init", 2);
  undefine_c_mod_predicate(module, "connect", 9);
  undefine_c_mod_predicate(module, "disconnect", 1);
  undefine_c_mod_predicate(module, "num_rows", 2);
  undefine_c_mod_predicate(module, "num_fields", 2);
  undefine_c_mod_predicate(module, "free_result", 1);
  undefine_c_mod_predicate(module, "query", 3);
  undefine_c_mod_predicate(module, "use_result", 2);
  undefine_c_mod_predicate(module, "fetch_row", 2);
  undefine_c_mod_predicate(module, "error_string", 2);
  undefine_c_mod_predicate(module, "nth_string", 3);
}

