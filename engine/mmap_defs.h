#if defined(HAS_MMAP)

# if defined(ANONYMOUS_MMAP)
#  if defined(DARWIN) // Different names for the flags + special flag
#   define MMAP_FLAGS (MAP_ANON|     MAP_PRIVATE|MAP_FIXED|MAP_HASSEMAPHORE)
#  elif defined(Solaris)
#   define MMAP_FLAGS (MAP_ANON|     MAP_PRIVATE|MAP_FIXED)
#  else
#   define MMAP_FLAGS (MAP_ANONYMOUS|MAP_PRIVATE|MAP_FIXED)
#  endif
# else
#  define MMAP_FLAGS  (              MAP_PRIVATE|MAP_FIXED)
#  define MMAP_FILE   "/dev/zero"
# endif

# include <sys/mman.h>

# if defined(DARWIN)
#  include <sys/types.h>
# endif

#endif
