/* 
 * locks.c -- various not-inline primitives for management of locks
 * Author          : Manuel Carro
 * Created On      : Wed Nov 19 20:03:55 1997
 * Last Modified By: MCL
 * Last Modified On: Tue May  2 18:04:32 2000
 * Update Count    : 209
 * Status          : Unknown, Use with caution!
 */


#include "datadefs.h"                      
#include "support.h"                      

/* declarations for global functions accessed here */

#include "locks_defs.h"
#include "alloc_defs.h"

/* local declarations */


#if defined(HAVE_LIB_LOCKS) && defined(DEBUG)
#if defined(Win32)
BOOL lock_is_unset_win32(p)
     LOCK *p;
{
  int value;
  if ((value = TryEnterCriticalSection(p)))  /* value != 0 -> lock not set */
    LeaveCriticalSection(p);
  return value;
}
#else
BOOL lock_is_unset(p)
     LOCK *p;
{
  int value;
  if ((value = pthread_mutex_trylock(p)) != EBUSY)
    pthread_mutex_unlock(p);
  return (value != EBUSY);
}
#endif
#endif

#if defined(USE_LOCKS)
#if defined(GENERAL_LOCKS)
 /* Implementation of general locks based on binary ones (Barz, 1983,
    SIGPLAN Notices) */

/* lock_atom/1: puts a lock on X(0), which must be an atom */

BOOL prolog_lock_atom(Arg)
     Argdecl;
{
  TAGGED term;
  struct atom *atomptr;

  DEREF(term, X(0));

  if (TagIsATM(term)) {                                    /* Atom -- lock */
    atomptr = TagToAtom(term);
    Wait_Acquire_lock(atomptr->atom_lock_l);
    Wait_Acquire_slock(atomptr->counter_lock);
    atomptr->atom_lock_counter--;
    if (atomptr->atom_lock_counter > 0)
      Release_lock(atomptr->atom_lock_l);
    Release_slock(atomptr->counter_lock);
  } else BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(0),1);

  return TRUE;
}


BOOL prolog_unlock_atom(Arg)                                     /* Ditto */
     Argdecl;
{
  TAGGED term;
  struct atom *atomptr;

  DEREF(term, X(0));

  if (TagIsATM(term)) {
    atomptr = TagToAtom(term);
    Wait_Acquire_slock(atomptr->counter_lock);
    atomptr->atom_lock_counter++;
    if (atomptr->atom_lock_counter == 1)
      Release_lock(atomptr->atom_lock_l);
    Release_slock(atomptr->counter_lock);
  } else BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(0),1);

  return TRUE;
}

BOOL prolog_lock_atom_state(Arg)                                 /* Ditto */
     Argdecl;
{
  TAGGED term, value;
  struct atom *atomptr;
  int          lock_value;

  DEREF(term, X(0));

  if (TagIsATM(term)) {
    atomptr = TagToAtom(term);
    DEREF(value, X(1));
    if (TagIsSmall(value)) {
      Wait_Acquire_slock(atomptr->counter_lock);
      atomptr->atom_lock_counter = GetSmall(value);
      Release_slock(atomptr->counter_lock);
      return TRUE;
    }
    else if (IsVar(value)) {
      Wait_Acquire_slock(atomptr->counter_lock);
      lock_value = atomptr->atom_lock_counter;
      Release_slock(atomptr->counter_lock);
      return cunify(Arg, X(1), MakeSmall(lock_value));
    }
    else BUILTIN_ERROR(TYPE_ERROR(VARIABLE),X(1),2);
  } else BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(0),1);
}

#else                                                    /* GENERAL_LOCKS */

/* lock_atom/1: puts a lock on X(0), which must be an atom */

BOOL prolog_lock_atom_bin(Arg)
     Argdecl;
{
  TAGGED term;
  struct atom *atomptr;

  DEREF(term, X(0));

  if (TagIsATM(term)) {                                    /* Atom -- lock */
    atomptr = TagToAtom(term);
    Wait_Acquire_lock(atomptr->atom_lock_l);
  } else BUILTIN_ERROR(TYPE_ERROR(ATOM),X(0),1);

  return TRUE;
}

BOOL prolog_unlock_atom_bin(Arg)                                 /* Ditto */
     Argdecl;
{
  TAGGED term;
  struct atom *atomptr;

  DEREF(term, X(0));

  if (TagIsATM(term)) {
    atomptr = TagToAtom(term);
    Release_lock(atomptr->atom_lock_l);
  } else BUILTIN_ERROR(TYPE_ERROR(ATOM),X(0),1);

  return TRUE;
}

#endif                                                   /* GENERAL_LOCKS */

 /* Releases the lock on a predicate; this is needed to ensure that a clause
    will not be changed while it is being executed. */

BOOL prolog_unlock_predicate(Arg)
     Argdecl;
{
  struct int_info *root = TagToRoot(X(0));

#if defined(DEBUG)
  if (debug_conc && 
      root->behavior_on_failure == CONC_OPEN &&
      Cond_Lock_is_unset(root->clause_insertion_cond))
    fprintf(stderr,
      "WARNING: In unlock_predicate, root is %x, predicate is unlocked!!!!\n", 
            (unsigned int)root);
#endif

  if (root->behavior_on_failure != DYNAMIC)
    Wait_For_Cond_End(root->clause_insertion_cond);

  return TRUE;
}

#else                                                        /* !USE_LOCKS */

/* lock_atom/1: puts a lock on X(0), which must be an atom */

BOOL prolog_lock_atom(Arg)
     Argdecl;
{ return TRUE;}
BOOL prolog_lock_atom_state(Arg)
     Argdecl;
{ return TRUE;}
BOOL prolog_unlock_atom(Arg)                                     /* Ditto */
     Argdecl;
{ return TRUE;}
/*
void init_dynamic_locks() {}
LOCK create_dynamic_lock(void){return NULL;}
*/
BOOL prolog_unlock_predicate(Arg)
     Argdecl;
{
#if defined(DEBUG)
  if (debug_conc) fprintf(stderr, "Using fake unlock_predicate!!!!\n");
#endif
  return TRUE;
}

#endif

/***************************************************************************/

#if defined(DEBUG)

unsigned long int ops_counter = 0;
SLOCK ops_counter_l;

unsigned long int get_inc_counter()
{
  unsigned long int local_counter;
  Wait_Acquire_slock(ops_counter_l);
  local_counter = ops_counter++;
  Release_slock(ops_counter_l);
  return local_counter;
}

void reset_counter()
{
  Wait_Acquire_slock(ops_counter_l);
  ops_counter = 0;
  Release_slock(ops_counter_l);
}

#endif
