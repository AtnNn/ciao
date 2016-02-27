/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#include <sys/types.h>

#include "datadefs.h"
#include "support.h"

/* declarations for global functions accessed here */

#include "timing_defs.h"

/* local declarations */


#if (defined(Solaris) || defined(SunOS4) || defined(LINUX) || defined(DARWIN) || defined(IRIX) || defined(Win32)) && !defined(crossWin32i86)

#include <sys/time.h>
#include <sys/resource.h>


ENG_FLT usertime()
{
  struct rusage rusage;
  
  getrusage(RUSAGE_SELF,&rusage);
  return (ENG_FLT)rusage.ru_utime.tv_sec+(ENG_FLT)rusage.ru_utime.tv_usec/1.0e6;
}

#else

#include <sys/times.h>
#include <sys/param.h>

ENG_FLT usertime()
{
  struct tms buffer;
  
  times(&buffer);
  return (ENG_FLT)buffer.tms_utime/(ENG_FLT)HZ;
}

#endif

/* runtime returns a list of two integer 
 * giving time in milliseconds. The first number gives time from the system 
 * start_up and the  since the last call to runtime */
BOOL prolog_runtime(Arg)
     Argdecl;
{
  int st,lt;
  ENG_FLT t;
  TAGGED x;
  
  t = usertime();
  st = (t - stats.starttime)*1000;
  lt = (t - stats.lasttime)*1000;
  stats.lasttime = t;
  MakeLST(x,MakeInteger(Arg,lt),atom_nil);
  MakeLST(x,MakeInteger(Arg,st),x);
  return cunify(Arg,x,X(0));
}

extern time_t time PROTO((time_t *));

BOOL prolog_time(Arg)
     Argdecl;
{
  
  time_t timeofday = time(NULL);

  return cunify(Arg,MakeInteger(Arg,timeofday),X(0));
}


/* walltime(?Time): unifies Time with the time in milliseconds elapsed since
  the last call to walltime/1 . The first call returns walltime since the
  start of the execution.  */

/* Shared but locked?  Initialized in init_once() */

/*
long last_walltime_secs;      
long last_walltime_usecs;
long initial_walltime_secs;   
long initial_walltime_usecs;  
*/

/*
BOOL prolog_walltime(Arg)
     Argdecl;
{
  long this_time_secs;
  long this_time_usecs;
  struct timeval tp;
  long delta_time;
  
  gettimeofday(&tp, 0L);

  this_time_secs  = tp.tv_sec;
  this_time_usecs = tp.tv_usec;

  delta_time = (this_time_secs - last_walltime_secs) * 1000 +
                (this_time_usecs / 1000 - last_walltime_usecs / 1000);
  last_walltime_secs = this_time_secs;
  last_walltime_usecs = this_time_usecs;
  
  return cunify(Arg, MakeInteger(Arg,delta_time), X(0));
}
*/

ENG_FLT walltime()
{
    struct timeval tp;

    gettimeofday(&tp, 0L);
    return (ENG_FLT)tp.tv_sec*1000 + (ENG_FLT)tp.tv_usec/1000;
}


BOOL prolog_walltime(Arg)
     Argdecl;
{
  int st,lt;
  ENG_FLT t;
  TAGGED x;
  
  t = walltime();
  st = (ENG_INT)(t - stats.startwalltime);
  lt = (ENG_INT)(t - stats.lastwalltime);
  stats.lastwalltime = t;
  MakeLST(x,MakeInteger(Arg,lt),atom_nil);
  MakeLST(x,MakeInteger(Arg,st),x);
  return cunify(Arg,x,X(0));
}

/* datime(+Time,-Year,-Month,-Day,-Hour,-Min,-Sec,-WeekDay,-YearDay) */

BOOL prolog_datime(Arg)
     Argdecl;
{
  struct tm *datime;
  time_t inputtime[1];

  DEREF(X(0),X(0));

  if (IsVar(X(0))) {
    inputtime[0] = time(NULL);
    cunify(Arg,MakeInteger(Arg,inputtime[0]),X(0));
  } else if (IsInteger(X(0))) {
    inputtime[0] = GetInteger(X(0));
  } else
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(0),1);

  datime = localtime(inputtime);

  return(cunify(Arg,MakeSmall((datime->tm_year)+1900),X(1))
      && cunify(Arg,MakeSmall((datime->tm_mon)+1), X(2))
      && cunify(Arg,MakeSmall(datime->tm_mday),X(3))
      && cunify(Arg,MakeSmall(datime->tm_hour),X(4))
      && cunify(Arg,MakeSmall(datime->tm_min), X(5))
      && cunify(Arg,MakeSmall(datime->tm_sec), X(6))
      && cunify(Arg,MakeSmall(datime->tm_wday),X(7))
      && cunify(Arg,MakeSmall(datime->tm_yday),X(8)));

}

