#include "/home/clip/Systems/ciao/include/LINUXi86/datadefs.h"
#include "/home/clip/Systems/ciao/include/LINUXi86/support.h"
#include "/home/clip/Systems/ciao/lib/foreign_interface/foreign_interface.h"

double sin(double);
BOOL prolog_sin(struct worker *w) {
  double v0;
  double v1;
  TAGGED p1;
  DEREF(X(0), X(0));
  GET_NUMBER(0, v0);
  v1 = sin(v0);
  p1 = MakeFloat(Arg, v1);
  DEREF(X(1), X(1));
  if (!cunify(Arg, X(1), p1)) return FALSE;
}

double cos(double);
BOOL prolog_cos(struct worker *w) {
  double v0;
  double v1;
  TAGGED p1;
  DEREF(X(0), X(0));
  GET_NUMBER(0, v0);
  v1 = cos(v0);
  p1 = MakeFloat(Arg, v1);
  DEREF(X(1), X(1));
  if (!cunify(Arg, X(1), p1)) return FALSE;
}

double fabs(double);
BOOL prolog_fabs(struct worker *w) {
  double v0;
  double v1;
  TAGGED p1;
  DEREF(X(0), X(0));
  GET_NUMBER(0, v0);
  v1 = fabs(v0);
  p1 = MakeFloat(Arg, v1);
  DEREF(X(1), X(1));
  if (!cunify(Arg, X(1), p1)) return FALSE;
}

void math_init(char *module) {
  FUNCTOR_DEFINITION_CODE;
  define_c_mod_predicate(module, "sin", prolog_sin, 2);
  define_c_mod_predicate(module, "cos", prolog_cos, 2);
  define_c_mod_predicate(module, "fabs", prolog_fabs, 2);
}

void math_end(char *module) {
  undefine_c_mod_predicate(module, "sin", 2);
  undefine_c_mod_predicate(module, "cos", 2);
  undefine_c_mod_predicate(module, "fabs", 2);
}

