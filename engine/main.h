#include <sys/param.h>

#include "datadefs.h"


#if defined(DEBUG)
int debug_c = 0;
#endif

#if !defined(MAXPATHLEN)
# define MAXPATHLEN 1024
#endif

char *pathname=NULL;
/*char *emulator_path; -- Unused now. DCG */ /* absolute path name -- Shared */
char source_path[MAXPATHLEN] = "";                             /* Shared */
int prolog_force_interactive = 0;      /* Shared --- not really relevant? */

#if defined(Win32)
char library_directory[MAXPATHLEN+1] = "";
#else
char *library_directory = NULL;
#endif

#if defined(__svr4__)                                          /* Solaris */
#include <unistd.h>                                            /* sbrk () */
#include <stdlib.h>                                           /* malloc() */
#else                                                            /* SunOS */
#include <sys/types.h>
#include <malloc.h>
#endif

#if defined(MALLOC_DEBUG)
#include "dmalloc.h"
#endif

#if !defined(X_OK)
# define X_OK 1
#endif
