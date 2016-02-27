
#if !defined(_LOCKS_H_)


#define _LOCKS_H_                           /* avoid multiple definitions */


/* If we undefine this, only semaphores will be available */

#define GENERAL_LOCKS

#if !defined(NULL)
#define NULL (void *)0
#endif

#if defined(POSIX_LOCKS) && defined(THREADS)
#include <semaphore.h>     
typedef sem_t LOCK_ST;
typedef LOCK_ST *LOCK;
#else
typedef long int Int;                 /* basic integer, size == word size */
typedef volatile unsigned long int LOCK_ST;
typedef LOCK_ST *LOCK;
/*
typedef unsigned long int Int;
typedef volatile Int LOCK_ST;
typedef LOCK_ST *LOCK;
*/
#endif

/* Struct to hold an array of locks; several of them can be linked together */

#define LOCK_BLOCK_SIZE 128

typedef struct LOCK_BLOCK {
  LOCK_ST lockarray[LOCK_BLOCK_SIZE];
  int current_index;
  struct LOCK_BLOCK *next;
} *LOCK_BLOCK_P;
  



#if !defined(THREADS)                                   /* Empty macros */

#define Init_lock(p)          
#define Wait_Acquire_lock(p)  
#define Release_lock(p)       
#define Lock_is_unset(p)      1

/*
#define Acquire_lock_or_fail(p) 
#define Try_Acquire_self_lock(p)
#define Reset_lock(p, value) 
#define Release_self_lock(p)
*/

#else 

#if !defined(POSIX_LOCKS)

/* Kish' implementation */

/*
#if defined(UNDEF)
#if defined(sparc) || defined(sequent) 
#define DESTRUCTIVE 1 
#endif
#endif
*/

#if defined(DESTRUCTIVE)
#define lock_offset 1 /*lock is offset from start of structure by one word*/
#define LockOffSet(p) (((LOCK_ST *)(p)) + lock_offset)
#else                                                     /* !DESTRUCTIVE */
#define LockOffSet(p) ((LOCK_ST *)(p))
#endif                                                     /* DESTRUCTIVE */

                   /* Macro definition for sparc */
#  if defined(Sparc)     
#    ifdef __GNUC__

/* uses GNU C's interface */
#    define aswap(addr,reg)                                     \
({ int _ret;                                                    \
   asm volatile ("swap %1,%0"                                   \
        : "=r" (_ret), "=m" (*(addr))    /* Output %0,%1 */     \
        : "m"  (*(addr)), "0" (reg));    /* Input (%2),%0 */    \
   _ret;                                                        \
})

#    else                                                    /* !__GNUC__ */

#    define aswap(addr,reg) Swap(addr,reg)

#    endif                                                    /* __GNUC__ */
#  endif                                                         /* sparc */


                           /* Now, Intel 80x86 stuff */

#  if defined(i86)

#  define aswap(adr,reg)                                        \
  ({ Int _ret;                                                  \
     asm volatile ("xchgw %0,%1"                                \
        : "=q" (_ret), "=m" (*(adr))    /* Output %0,%1 */      \
        : "m"  (*(adr)), "0"  (reg));   /* Input (%2),%0 */     \
     _ret;                                                      \
  })

#  endif


/* We have to make sure that lock operations do not get removed by a
   ultra-smart compiler.  barrier(), taken from Linux kernel code, ensures
   that any values the compiler remembered don't get reused after the lock
   is acquired. We want one before the unlock too - so as to force a
   writeback of values. That gives us synchronization points at the locks.  */

/*#define barrier() __asm__("": : :"memory")   Not working.... :-( */


#define Init_lock(p)             *(LockOffSet(p))=0

/*
#define Wait_Acquire_lock(p)         \
{                                    \
    while (aswap((LockOffSet(p)),1)) \
      ;                              \
}
*/

/*
  From alan@lxorguk.ukuu.org.uk: 

  Whenever the lock grab fails the CPU will spin sampling the cache
  line until another CPU writes it back which invalidates our MESI
  cache line for the data. This avoids the code spin-generating lock
  cycles on the bus.

  From an experiment involving access to a shared predicate, this option
  gives much better performance than POSIX locks (with the difference being
  more acute the more agents there are).  The alternative native locks are
  faster for one and two concurrent agents looking at the same predicate,
  but they get slightly behind in performance from three agents onwards.
  MCL.
*/

#define Wait_Acquire_lock(p) while (aswap((LockOffSet(p)),1)) {while(*p);}

#define Release_lock(p)          *(LockOffSet(p))=0
#define Lock_is_unset(p)         *(LockOffSet(p))==0

/*
#define Acquire_lock_or_fail(p)  aswap((LockOffSet(p)),1)==0
#define Try_Acquire_self_lock(p) (aswap(*(LOCK_ST **)p, *(LOCK_ST *)p))
#define Reset_lock(p, value)     (aswap(*(LOCK_ST **)p, value))
#define Release_self_lock(p)
*/


#else                                                   /*  (POSIX_LOCKS) */

#define Init_lock(p)             sem_init(p, 0, 1)
#define Wait_Acquire_lock(p)     sem_wait(p)
#define Release_lock(p)          sem_post(p)
#define Lock_is_unset(p)         lock_is_unset(p)

#endif                                                  /*  (POSIX_LOCKS) */
#endif                                                       /* THREADS */
#endif                                             /* !defined(_LOCKS_H_) */


#if defined(DEBUG)
#define GET_INC_COUNTER get_inc_counter()
#define RESET_COUNTER reset_counter()
#else
#define GET_INC_COUNTER 0
#define RESET_COUNTER 
#endif
