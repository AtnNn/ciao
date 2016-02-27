
/* Disallow threads in selected cases, even if set at the general settings */

#if defined(THREADS) 

/* These macros allows the same macros to be use with the POSIX and Solaris 
   threads packages.  Some code taken from Kish's DASWAM
*/

#if defined(LINUX) || defined(IRIX) || defined(Win32)

/* #define _MIT_POSIX_THREADS 1 */
#include <pthread.h>

typedef pthread_t THREAD_T;

#define Thread_Create_NoGoalId(Process, Arg, Id) \
    pthread_create(Id, &detached_thread, Process, Arg)

#define Thread_Create_GoalId(Process, Arg, Id) \
    pthread_create(Id, &joinable_thread, Process, Arg)

#define Thread_Join(Id) pthread_join(Id, NULL)
#define Thread_Exit(Status)     pthread_exit(Status)
#define Thread_Id           pthread_self()
#define Allow_Thread_Cancel \
     pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL); \
     pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
#define Disallow_Thread_Cancel \
     pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, NULL); \
     pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, NULL);
#define Thread_Cancel(Id) pthread_cancel(Id)
#define Thread_Equal(thr1, thr2) pthread_equal(thr1, thr2)
#endif                                                       /* __linux__ */


#if defined(sequent)

# if !defined(__GNUC__)
# include <parallel/microtask.h>
# else                 /* gcc does not like the syntax inside microtask.h */
  extern int m_myid;		                 /* my process identifier */
# endif                                                       /* __GNUC__ */

#define Thread_Exit(status) exit(status)
/* initialise to die flag, which is set to 1 when worker should terminate */
#define Allow_Thread_Cancel worker->to_die = 0;
#endif                                                         /* sequent */




#if defined(Solaris)

 /* Two possible APIs: Solaris threads and POSIX threads.  Choose. */

#if defined(USE_SOLARIS_THREADS)

#include <signal.h>

#include <thread.h>

typedef thread_t THREAD_T;


#define Thread_Create(Process, Arg, Id) \
  thr_create(NULL, 0, Process, Arg, THR_NEW_LWP, Id)
/*#define Thread_Create_no_Id(Process, Arg) Thread_Create(Process,Arg,NULL)*/
#define Thread_Join(i) thr_join(0, NULL, NULL)
#define Thread_Exit(status) thr_exit(status)
#define Thread_Id           thr_self()
#define Allow_Thread_Cancel 
#define Disallow_Thread_Cancel 
#define Thread_Cancel(Id) thr_kill(Id, SIGTERM)
#define Thread_Equal(thr1, thr2) thr_equal(thr1, thr2)     
#endif

#if defined(USE_POSIX_THREADS)

#include <pthread.h>

typedef pthread_t THREAD_T;

 /* We want threads to be scheduled on a system-wide basis (i.e., every
    thread is given the same opportunities to run as any process in the
    system) */


#define Thread_Create_NoGoalId(Process, Arg, Id) \
    pthread_create(Id, &detached_thread, Process, Arg)

#define Thread_Create_GoalId(Process, Arg, Id) \
    pthread_create(Id, &joinable_thread, Process, Arg)

#define Thread_Join(Id) pthread_join(Id, NULL)
#define Thread_Exit(Status) pthread_exit(Status)
#define Thread_Id           pthread_self()
#define Allow_Thread_Cancel                                   \
     pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL); \
     pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
#define Disallow_Thread_Cancel \
     pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, NULL); \
     pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, NULL);
#define Thread_Cancel(Id) pthread_cancel(Id)
#define Thread_Equal(thr1, thr2) pthread_equal(thr1, thr2)
#endif                                                   /* POSIX threads */
#endif                                                           /* sparc */


#else  /* !defined(THREADS) */

#include <unistd.h>

#if defined(IRIX)
# include <sys/types.h>
# include <signal.h>
#endif

#if defined(SunOS4)
#include <sys/wait.h>
#endif

#if defined(Solaris) || defined(LINUX)
#include <sys/types.h>
#include <sys/wait.h>
#endif

typedef int THREAD_T;

#define Thread_Id getpid()
#define Thread_Create_GoalId(Process, Arg, Id) \
      if (Id!=NULL) *Id = getpid(); Process (Arg)
/*#define Thread_Create_no_Id(Process, Arg) Process (Arg)*/
#define Thread_Join(i) 
#define Thread_Exit(status)    exit(0)           /* Not correct, actually */
#define Allow_Thread_Cancel 
#define Disallow_Thread_Cancel 
#define Thread_Cancel(Id) kill(Thread_Id, SIGTERM)
#define Thread_Equal(thr1, thr2) (thr1 == thr2)
#endif /* defined(THREADS) */
