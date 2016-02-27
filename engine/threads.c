#if defined(THREADS)

#include "datadefs.h"
#include "support.h"
#include "threads.h"



/* Start a new thread, build its own WAM areas, add to the list of WAMs,
 jump to execution in the predicate passed on in X(0). */

BOOL prolog_start_thread(Arg)
     Argdecl;
{

  TAGGED jump_to;
  TAGGED definition;
  unsigned char *predicate_name;

  DEREF(jump_to, X(0));

  predicate_name = GetString(jump_to);

  definition = init_atom_check(predicate_name);

  printf("Calling %s\n", predicate_name);

 /* Now, just create a wam for ourselves; later, we will look for a free one */

  return TRUE;
}



BOOL prolog_exit_thread(Arg)
     Argdecl;
{}

#endif
