#include <stdlib.h>

#include "/home/clip/Systems/ciao/bin/LINUXi86/datadefs.h"
#include "/home/clip/Systems/ciao/bin/LINUXi86/support.h"
/*#include "common_headers.h"*/

struct try_node *address_i_am_native_2 = NULL;

float r = 1.0;

BOOL prolog_i_am_native_2(Arg)
     Argdecl;
{
  r *= 4;
  if (r > 1000) {
    pop_choicept(Arg);
    return FALSE;
  }
  DEREF(X(0),X(0));
  return cunify(Arg, MakeFloat(Arg, r), X(0));
}

BOOL prolog_i_am_native(Arg)
     Argdecl;
{
  if (!address_i_am_native_2) {
    address_i_am_native_2 = def_retry_c(prolog_i_am_native_2, 1);
  }
  push_choicept(Arg, address_i_am_native_2);
  push_frame(Arg, 0);
  DEREF(X(0),X(0));
  pop_frame(Arg, 0);
  return cunify(Arg, MakeFloat(Arg, r=1.0), X(0));
}


