/*#define USE_OWN_MALLOC_SOLARIS */            /* Optional, to test impl. */
#define USE_OWN_MALLOC_LINUX         /* Linux malloc breaks: use own m.m. */
/*#define USE_OWN_MALLOC_IRIX*/
/*#define USE_OWN_MALLOC_NT*/
/*#define USE_OWN_MALLOC_SUNOS*/

#if defined(LINUX) || defined(Solaris) 
#  include <stdlib.h>
#endif

#if defined(SunOS4)
#  include <malloc.h>
#endif                                                           /* SunOS */

#if (defined(USE_OWN_MALLOC_LINUX) && defined(LINUX)) || (defined (Solaris) && defined(USE_OWN_MALLOC_SOLARIS)) || (defined(SunOS4) && defined(USE_OWN_MALLOC_SUNOS)) || (defined(USE_OWN_MALLOC_IRIX) && defined(IRIX)) || (defined(USE_OWN_MALLOC_Win32) && defined(Win32))
#  define USE_OWN_MALLOC
#  define Malloc(p)     own_malloc(p)
#  define Free(p)       own_free(p)
#  define Realloc(p, s) own_realloc(p, s)
#else
#  define Malloc(p)     malloc(p)
#  define Free(p)       free((char *)p)
#  define Realloc(p, s) realloc((char *)p, s)
#endif                                                       



#if defined(sequent) 
#  define Malloc(p) shmalloc(p)
#  define Free(p)   free(p)
#  define Realloc(p, s) realloc(p, s)
#endif                                                         /* sequent */


#if defined(USE_OWN_MALLOC)
TAGGED *own_malloc(int size);
TAGGED *own_realloc(TAGGED *ptr, int size_in_chars);
void own_free(TAGGED *ptr);
void init_mm(void);
#endif
