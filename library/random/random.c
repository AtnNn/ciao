
#include <stdlib.h>

#include "common_headers.h"
 

#if defined(Solaris)
long random();
void srandom( unsigned seed);
#endif


#if defined(LINUX)
#  define RANDOM_MAX RAND_MAX
#else
#  define RANDOM_MAX 2147483647
#endif

/* This is for RANDOM in [0 1)
#define RANDOM ((ENG_FLT) -random()/(-RANDOM_MAX-1))
*/

/* This is for RANDOM in [0 1] */
#define RANDOM ((ENG_FLT) random()/RANDOM_MAX)

BOOL prolog_random(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));

  if (!IsVar(X(0)))
    BUILTIN_ERROR(INSTANTIATION_ERROR,atom_nil,1)
  
  return cunify(Arg,MakeFloat(Arg,RANDOM),X(0));
}

BOOL prolog_random3(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  if (!IsNumber(X(0)))
    ERROR_IN_ARG(X(0),1,NUMBER)

  DEREF(X(1),X(1));
  if (!IsNumber(X(1)))
    ERROR_IN_ARG(X(1),1,NUMBER)

  DEREF(X(2),X(2));
  if (!IsVar(X(2)))
    BUILTIN_ERROR(INSTANTIATION_ERROR,atom_nil,3)

  if (IsInteger(X(0)) && IsInteger(X(1))) {
    ENG_INT low = GetInteger(X(0));
    ENG_INT up  = GetInteger(X(1));
    return(cunify(Arg, MakeInteger(Arg, low+(random() % (up-low+1))), X(2)));
  } else{
    ENG_FLT low = GetFloat(X(0));
    ENG_FLT up  = GetFloat(X(1));
    return(cunify(Arg, MakeFloat(Arg, low+RANDOM*(up-low)), X(2)));
  }
}

BOOL prolog_srandom(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));

  if (IsVar(X(0)))
    srandom(1);
  else if (IsInteger(X(0)))
    srandom((int)CTagToArg(X(0),1));
  else
    ERROR_IN_ARG(X(1),1,INTEGER);

  return(TRUE);
}

void random_init(module)
     char *module;
{
  define_c_mod_predicate(module, "random", prolog_random, 1);
  define_c_mod_predicate(module, "random", prolog_random3, 3);
  define_c_mod_predicate(module, "srandom", prolog_srandom, 1);
}

void random_end(module)
     char *module;
{
  undefine_c_mod_predicate(module, "random",  1);
  undefine_c_mod_predicate(module, "random", 3);
  undefine_c_mod_predicate(module, "srandom", 1);
}

