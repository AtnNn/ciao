/* 
 * locks.c -- various not-inline primitives for management of locks
 * Author          : Manuel Carro
 * Created On      : Wed Nov 19 20:03:55 1997
 * Last Modified By: MCL
 * Last Modified On: Fri Jul  2 17:38:08 1999
 * Update Count    : 86
 * Status          : Unknown, Use with caution!
 */


#include "datadefs.h"                      
#include "support.h"                      

/* declarations for global functions accessed here */

#include "locks_defs.h"
#include "alloc_defs.h"

/* local declarations */

#if defined(THREADS)
static LOCK_BLOCK_P new_lock_block(LOCK_BLOCK_P old_block);


static LOCK    locks_pool_l = NULL;
static LOCK_ST locks_pool_st;

static LOCK_BLOCK_P dynamic_block_list = NULL;

 /* Create a new block of locks and make it point to old_block to maintain
    the linked list. */

static LOCK_BLOCK_P new_lock_block(old_block)
     LOCK_BLOCK_P old_block;
{
  LOCK_BLOCK_P new_block;
  
  new_block = (LOCK_BLOCK_P)checkalloc(sizeof(struct LOCK_BLOCK));
  new_block -> current_index = 0;
  new_block -> next = old_block;
  return new_block;
}


 /* Create the first block, with no initialized blocks */

void init_dynamic_locks()
{
  locks_pool_l = &locks_pool_st;
  Init_lock(locks_pool_l);
  dynamic_block_list = new_lock_block(dynamic_block_list);
}


/* Returns a pointer to a new lock storage location, which is already
   inited.  As we (by now) do not remove blocks/erase locks already
   provided, if there is a block with free storage, it must be the first
   one.  Since we may change a global variable, we must lock the whole
   operation. */

LOCK create_dynamic_lock()
{
  LOCK new_lock;

  Wait_Acquire_lock(locks_pool_l);

  if (dynamic_block_list->current_index == LOCK_BLOCK_SIZE)/* Full. Create. */
    dynamic_block_list = new_lock_block(dynamic_block_list);

  new_lock = 
    &dynamic_block_list->lockarray[dynamic_block_list->current_index++];

  Release_lock(locks_pool_l);                   /* new_lock is ours, now. */

  Init_lock(new_lock);

  return new_lock;
}

#if defined(POSIX_LOCKS)
BOOL lock_is_unset(p)
     LOCK p;
{
  int value;
  sem_getvalue(p, &value);
  return value;
}
#endif

#if defined(GENERAL_LOCKS)
 /* Implementation of general locks based on binary ones */

/* lock_atom/1: puts a lock on X(0), which must be an atom */

BOOL prolog_lock_atom(Arg)
     Argdecl;
{
  TAGGED term;
  struct atom *atomptr;
  BOOL   i_must_wait = TRUE;

  DEREF(term, X(0));

  if (TagIsATM(term)) {                                    /* Atom -- lock */
    atomptr = TagToAtom(term);
    while(i_must_wait) {
      Wait_Acquire_lock(atomptr->atom_lock_l);
      if (atomptr->atom_lock_counter > 0){
        atomptr->atom_lock_counter--;
        Release_lock(atomptr->atom_lock_l);
        i_must_wait = FALSE;
      } else {
        Release_lock(atomptr->atom_lock_l);
        while (atomptr->atom_lock_counter < 1);  /* Spin lock on local cache */
      }
    }
  } else BUILTIN_ERROR(TYPE_ERROR(ATOM),X(0),1);

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
    Wait_Acquire_lock(atomptr->atom_lock_l);
    atomptr->atom_lock_counter++;
    Release_lock(atomptr->atom_lock_l);
  } else BUILTIN_ERROR(TYPE_ERROR(ATOM),X(0),1);

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
    DEREF(value, X(1));
    if (TagIsSmall(value)) {
      atomptr = TagToAtom(term);
      Wait_Acquire_lock(atomptr->atom_lock_l);
      atomptr->atom_lock_counter = GetSmall(value);
      Release_lock(atomptr->atom_lock_l);
    }
    else if (IsVar(value)) {
      atomptr = TagToAtom(term);
      Wait_Acquire_lock(atomptr->atom_lock_l);
      lock_value = atomptr->atom_lock_counter;
      Release_lock(atomptr->atom_lock_l);
      return cunify(Arg, X(1), MakeSmall(lock_value));
    }
    else BUILTIN_ERROR(TYPE_ERROR(VARIABLE),X(1),2);
  } else BUILTIN_ERROR(TYPE_ERROR(ATOM),X(0),1);
  return TRUE;
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

  if (root->behavior_on_failure != DYNAMIC)
    Release_lock(root->clause_lock_l);
  return TRUE;
}

#else                                                        /* !USE_LOCKS */


/* lock_atom/1: puts a lock on X(0), which must be an atom */

BOOL prolog_lock_atom(Arg)
     Argdecl;
{
  return TRUE;
}


BOOL prolog_lock_atom_state(Arg)
     Argdecl;
{
  return TRUE;
}


BOOL prolog_unlock_atom(Arg)                                     /* Ditto */
     Argdecl;
{
  return TRUE;
}

void init_dynamic_locks()
{
}

LOCK create_dynamic_lock(void)
{
  return NULL;
}

BOOL prolog_unlock_predicate(Arg)
     Argdecl;
{
  return TRUE;
}

#endif

/***************************************************************************/

#if defined(DEBUG)

unsigned long int ops_counter = 0;
LOCK_ST ops_counter_st;
LOCK ops_counter_l;

unsigned long int get_inc_counter()
{
  unsigned long int local_counter;
  Wait_Acquire_lock(ops_counter_l);
  local_counter = ops_counter++;
  Release_lock(ops_counter_l);
  return local_counter;
}

void reset_counter()
{
  Wait_Acquire_lock(ops_counter_l);
  ops_counter = 0;
  Release_lock(ops_counter_l);
}

#endif
