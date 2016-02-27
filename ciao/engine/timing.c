/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#include <string.h>

#include <sys/types.h>

#include "datadefs.h"
#include "support.h"

/* declarations for global functions accessed here */

#include "timing_defs.h"

/* local declarations */

#if (defined(Solaris) || defined(SunOS4) || defined(LINUX) || defined(DARWIN) \
        || defined(IRIX) || defined(Win32) || defined(BSD)) \
    && !defined(crossWin32i86)

#include <sys/time.h>
#include <sys/resource.h>

ENG_LINT internal_usertick_std(void)
{
  struct rusage rusage;
  getrusage(RUSAGE_SELF,&rusage);
  return ((ENG_LINT)rusage.ru_utime.tv_sec) * 1000000 + rusage.ru_utime.tv_usec;
}

ENG_LINT internal_systemtick_std(void)
{
  struct rusage rusage;
  getrusage(RUSAGE_SELF,&rusage);
  return ((ENG_LINT)rusage.ru_stime.tv_sec) * 1000000 + rusage.ru_stime.tv_usec;
}

static void init_frequency_info(void)
{
  ciao_statistics.userclockfreq = 1000000;
  ciao_statistics.systemclockfreq = 1000000;
}

#else

#include <sys/times.h>
#include <sys/param.h>

ENG_LINT internal_usertick_std(void)
{
  struct tms buffer;
  
  times(&buffer);
  return buffer.tms_utime;
}

ENG_LINT internal_systemtick_std(void)
{
  struct tms buffer;
  
  times(&buffer);
  return buffer.tms_stime;
}

static void init_frequency_info(void)
{
  ciao_statistics.userclockfreq = HZ;
  ciao_statistics.systemclockfreq = HZ;
}
#endif

/* usertick is defined as a pointer to a function to let the
   redefinition on the fly for a better timing measurement function */

ENG_LINT (*usertick)(void) = internal_usertick_std;
ENG_LINT (*systemtick)(void) = internal_systemtick_std;

ENG_FLT usertime(void)
{
  return ((ENG_FLT)usertick()) / ciao_statistics.userclockfreq;
}

extern time_t time PROTO((time_t *));

bool_t prolog_time(Arg)
     Argdecl;
{
  
  time_t timeofday = time(NULL);

  return cunify(Arg,MakeInteger(Arg,timeofday),X(0));
}


/* walltime(?Time): unifies Time with the time in milliseconds elapsed since
  the last call to walltime/1 . The first call returns walltime since the
  start of the execution.  */

/* Shared but locked?  Initialized in init_once() */

ENG_LINT internal_walltick_std(void)
{
  struct timeval tp;
  gettimeofday(&tp, 0L);
  return (ENG_LINT)tp.tv_sec*1000000 + ((ENG_LINT)tp.tv_usec);
}

ENG_LINT (*walltick)(void) = internal_walltick_std;

void init_statistics(void) {
  init_frequency_info();
  ciao_statistics.wallclockfreq = 1000000;
}

/*
  This function returns the walltime in milliseconds
*/

ENG_FLT walltime(void)
{
  return ((ENG_FLT)walltick() * 1000) / ciao_statistics.wallclockfreq;
}

/*
  This function has been modified by Edison Mera to prevents the
  truncation of the microseconds.  Very important in modern
  plattforms where the speed is given in GHz.
 */
static bool_t generic_time(
  Argdecl,
  ENG_LINT (*time_function)(void),
  ENG_LINT starttick,
  ENG_LINT *lasttick,
  ENG_LINT clockfreq)
{
  /*int st,lt; */
  ENG_LINT st,lt;
  ENG_LINT t;
  tagged_t x;
  
  t = time_function();
  st = t - starttick;
  lt = t - *lasttick;
  *lasttick = t;
  /* while ciao not support ENG_LINT, lt and st must be cast to
    ENG_FLT */
  MakeLST(x,MakeFloat(Arg,(((ENG_FLT)lt)*1000)/clockfreq),atom_nil);
  MakeLST(x,MakeFloat(Arg,(((ENG_FLT)st)*1000)/clockfreq),x);
  return cunify(Arg,x,X(0));
}

/* runtime returns a list of two floats
 * giving time in milliseconds. The first number gives time from the system 
 * start_up and the second since the last call to runtime */
bool_t prolog_runtime(Arg)
     Argdecl;
{
  return generic_time(Arg, 
                      TICK_FUNCTION,
                      ciao_statistics.starttick, 
                      &ciao_statistics.lasttick,
                      GET_CLOCKFREQ(ciao_statistics));
}

bool_t prolog_usertime(Arg)
     Argdecl;
{
  return generic_time(Arg, 
                      usertick,
                      ciao_statistics.startusertick,
                      &ciao_statistics.lastusertick,
                      ciao_statistics.userclockfreq);
}

bool_t prolog_systemtime(Arg)
     Argdecl;
{
  return generic_time(Arg, 
                      systemtick, 
                      ciao_statistics.startsystemtick,
                      &ciao_statistics.lastsystemtick,
                      ciao_statistics.systemclockfreq);
}

bool_t prolog_walltime(Arg)
     Argdecl;
{
  return generic_time(Arg, 
                      walltick, 
                      ciao_statistics.startwalltick, 
                      &ciao_statistics.lastwalltick,
                      ciao_statistics.wallclockfreq);
}

/* New time medition functions */
static bool_t generic_tick(
  Argdecl,
  ENG_LINT (*time_function)(void),
  ENG_LINT starttick,
  ENG_LINT *lasttick)
{
  /*int st,lt; */
  ENG_LINT st,lt;
  ENG_LINT t;
  tagged_t x;
  
  t = time_function();
  st = t - starttick;
  lt = t - *lasttick;
  *lasttick = t;
  /* while ciao does not support ENG_LINT, lt and st must be cast to
    ENG_FLT */
  MakeLST(x,MakeFloat(Arg,(ENG_FLT)lt),atom_nil);
  MakeLST(x,MakeFloat(Arg,(ENG_FLT)st),x);
  return cunify(Arg,x,X(0));  
}

bool_t prolog_walltick(Arg)
     Argdecl;
{
  return generic_tick(Arg, 
                       walltick, 
                       ciao_statistics.startwalltick,
                       &ciao_statistics.lastwalltick);
}

bool_t prolog_usertick(Arg)
     Argdecl;
{
  return generic_tick(Arg, 
                       usertick, 
                       ciao_statistics.startusertick,
                       &ciao_statistics.lastusertick);
}

bool_t prolog_systemtick(Arg)
     Argdecl;
{
  return generic_tick(Arg, 
                       systemtick, 
                       ciao_statistics.startsystemtick,
                       &ciao_statistics.lastsystemtick);
}

bool_t prolog_runtick(Arg)
     Argdecl;
{
  return generic_tick(Arg, 
                       TICK_FUNCTION,
                       ciao_statistics.starttick,
                       &ciao_statistics.lasttick);
}

/* New time medition functions */
inline static bool_t generic_clockfreq(
  Argdecl,
  ENG_LINT clockfreq)
{
  /* while ciao not support ENG_LINT, return value must be cast to
    ENG_FLT */
  return cunify(Arg,MakeFloat(Arg,(ENG_FLT)clockfreq),X(0));
}

bool_t prolog_runclockfreq(Arg)
     Argdecl;
{
  return generic_clockfreq(Arg, GET_CLOCKFREQ(ciao_statistics));
}

bool_t prolog_userclockfreq(Arg)
     Argdecl;
{
  return generic_clockfreq(Arg, ciao_statistics.userclockfreq);
}

bool_t prolog_systemclockfreq(Arg)
     Argdecl;
{
  return generic_clockfreq(Arg, ciao_statistics.systemclockfreq);
}

bool_t prolog_wallclockfreq(Arg)
     Argdecl;
{
  return generic_clockfreq(Arg, ciao_statistics.wallclockfreq);
}

void reset_statistics(void)
{
  ciao_statistics.startusertick = usertick();
  ciao_statistics.lastusertick = ciao_statistics.startusertick;
  ciao_statistics.startwalltick = walltick();
  ciao_statistics.lastwalltick = ciao_statistics.startwalltick;
  ciao_statistics.startsystemtick = systemtick();
  ciao_statistics.lastsystemtick = ciao_statistics.startsystemtick;
  ciao_statistics.lasttick = 
    ciao_statistics.starttick = 
    ciao_statistics.startusertick;
}

/* datime(+Time,-Year,-Month,-Day,-Hour,-Min,-Sec,-WeekDay,-YearDay) */
/* datime(-Time,-Year,-Month,-Day,-Hour,-Min,-Sec,-WeekDay,-YearDay) */
/* datime(-Time,+Year,+Month,+Day,+Hour,+Min,+Sec,-WeekDay,-YearDay) */

bool_t prolog_datime(Arg)
     Argdecl;
{
  ERR__FUNCTOR("system:datime", 9);
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));
  DEREF(X(4),X(4));
  DEREF(X(5),X(5));
  DEREF(X(6),X(6));
  
  if (IsInteger(X(1))
      && IsInteger(X(2))
      && IsInteger(X(3))
      && IsInteger(X(4))
      && IsInteger(X(5))
      && IsInteger(X(6))) {
    struct tm datime[1];
    time_t inputtime;
    datime->tm_year=GetInteger(X(1))-1900;
    datime->tm_mon =GetInteger(X(2))-1;
    datime->tm_mday=GetInteger(X(3));
    datime->tm_hour=GetInteger(X(4));
    datime->tm_min =GetInteger(X(5));
    datime->tm_sec =GetInteger(X(6));
    inputtime = mktime(datime);
    return(cunify(Arg,MakeInteger(Arg,inputtime),X(0))
	   && cunify(Arg,MakeSmall(datime->tm_wday),X(7))
	   && cunify(Arg,MakeSmall(datime->tm_yday),X(8)));
  } else {
    struct tm *datime;
    time_t inputtime;
    if (IsVar(X(0))) {
      inputtime = time(NULL);
      cunify(Arg,MakeInteger(Arg,inputtime),X(0));
    } else if (IsInteger(X(0))) {
      inputtime = GetInteger(X(0));
    } else {
      BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(0),1);
    }
    
    datime = localtime(&inputtime);
    
    return(cunify(Arg,MakeSmall((datime->tm_year)+1900),X(1))
	   && cunify(Arg,MakeSmall((datime->tm_mon)+1), X(2))
	   && cunify(Arg,MakeSmall(datime->tm_mday),X(3))
	   && cunify(Arg,MakeSmall(datime->tm_hour),X(4))
	   && cunify(Arg,MakeSmall(datime->tm_min), X(5))
	   && cunify(Arg,MakeSmall(datime->tm_sec), X(6))
	   && cunify(Arg,MakeSmall(datime->tm_wday),X(7))
	   && cunify(Arg,MakeSmall(datime->tm_yday),X(8)));
  }
}

#if defined(ANDPARALLEL) && defined(VISANDOR) && !defined(USCLK_EXISTS) && !defined(NSCLK_EXISTS)
/*
  We realized that perhaps the overhead caused by the call to
  usertime() could be too high to give us accurate timing. So we try to
  compensate it by counting the number of times called so far. 
  The initial compensate time is adjusted for an SUN SPARC.
  Things to do: a built-in predicate to return the compensating time.
*/

unsigned long int count_calls = 0; /* MCL */
float time_each_call = 0.000053;   /* For a SUN IPC */

/* usertime() returns seconds */

float usertime_visandor() {
  float time;

#if defined(Solaris)
  time = ((float)gethrtime()/1.0e9);
#else
  struct timeval current_time;

  gettimeofday(&current_time, NULL);
  time = current_time.tv_sec + current_time.tv_usec / 1e6;
#endif

  if (gen_event_file)
    time -= ++count_calls * time_each_call;

  return time;
}
#endif

