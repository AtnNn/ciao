#ifndef  _Settings_H_
# define _Settings_H_

/* # define PACK_PROFILE_DATA */

//#define PROFILE_TXT_OUTPUT // write profile information in text format

/* # define DEBUG_STACK */

/* # define DEBUG */
/* # define DEBUG_LEVEL 3 */

// define this to use the garbage collector
/* # define USE_GC */

// To watch the action of the garbage collector, define GC_DISPLAY.
/* # define GC_DISPLAY */

#define INLINE

# ifdef INLINE
#  define _INLINE inline
# else
#  define _INLINE
# endif

#endif	// _Settings_H_
