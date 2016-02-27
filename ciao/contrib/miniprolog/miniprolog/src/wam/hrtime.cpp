#if defined(__BORLANDC__)
#pragma hdrstop
#pragma package(smart_init)
#endif

#include "hrtime.h"

#if defined(__i386__) || defined(_M_IX86)

# if defined(__BORLANDC__)
# pragma warn -8070
# endif

# if defined(__BORLANDC__) || defined(_MSC_VER)

__declspec(naked) uint64 __cdecl hrtime(void) {
  __asm {
# if defined(SERIALIZE_WITH_CPUID)
    push ebx;
    cpuid  ;
    pop ebx;
# endif
    rdtsc  ;
    ret    ; //return value at EDX:EAX
  }
}

# endif

# if defined(__BORLANDC__)
# pragma warn +8070
# endif

#endif

/* more precise timing functions available in x86 plattform: tick
  represent the most precise time unit.  In Pentium II it is equal to
  1 cpu - cycle, however, in some modern platforms such equivalence is
  not straightforward.
*/

/* Returns the clock speed of the system's CPU in Hz, as reported by
  /proc/cpuinfo. On a multiprocessor machine, returns the speed of the
  first CPU. On error returns zero.

  Reference:

  http://www.informit.com/isapi/product_id~%7B480DF8FB-19B8-4C4E-88B8-FC2BF352887D%7D/content/index.asp

*/

#if defined(__i386__) || defined(__x86_64__)

#include <stdio.h>
#include <string.h>

uint64 hrfreq (void)
{
  FILE* fp;
  char buffer[4096];
  size_t bytes_read;
  char* match;
  double clock_speed;

  /* Read the entire contents of /proc/cpuinfo into the buffer. */
  fp = fopen ("/proc/cpuinfo", "r");
  bytes_read = fread (buffer, 1, sizeof (buffer), fp);
  fclose (fp);
  /* Fail if read failed or if buffer isn't big enough. */
  if (bytes_read == 0 || bytes_read == sizeof (buffer))
    return 0;
  /* NUL-terminate the text. */
  buffer[bytes_read] = '\0';
  /* Locate the line that starts with "cpu MHz". */
  match = strstr (buffer, "cpu MHz");
  if (match == NULL)
    return 0;
  /* Parse the line to extract the clock speed. */
  sscanf (match, "cpu MHz : %lf", &clock_speed);
  return (uint64)(clock_speed * 1000000);
}

#endif
