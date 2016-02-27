/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

#if defined(DEBUG)
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>                                    /* For getpid() */
#endif

#include <string.h>

#include "threads.h"                                     /* For debugging */
#include "datadefs.h"
#include "support.h"
#include "predtyp.h"


/* declarations for global functions accessed here */

#include "locks_defs.h"
#include "objareas_defs.h"
#include "alloc_defs.h"
#include "nondet_defs.h"
#include "stacks_defs.h"
#include "profile_defs.h"
#include "wam.h"

/* local declarations */

typedef enum {BLOCK, NO_BLOCK} BlockingType;

static unsigned int predicate_property_bits(register definition_t *d);
static instance_t *current_instance_noconc(Argdecl);

static instance_t *current_instance_conc(Argdecl, BlockingType block);

static bool_t wait_for_an_instance_pointer(instance_t **ins_pptr1,
					   instance_t **ins_pptr2,
					   int_info_t *root,
					   BlockingType block);

static instance_t *first_possible_instance(tagged_t head,
					   int_info_t *root,
					   instance_t **x2_n,
					   instance_t **x5_n);

static instance_handle_t *make_handle_to(instance_t *inst,
					 int_info_t *root,
					 tagged_t head,
					 WhichChain chain);

static void remove_handle(instance_handle_t *xi,
                          int_info_t *root,
                          WhichChain chain);

static void change_handle_to_instance(instance_handle_t *handle,
                                      instance_t *new_inst,
                                      int_info_t *root,
                                      WhichChain chain);

static void unlink_handle(instance_handle_t *xi, 
			  int_info_t *rt, 
			  WhichChain chain);

static void link_handle(instance_handle_t *handle,
			instance_t *inst,
			int_info_t *root,
			WhichChain chain);

#define E		((frame_t *)pt1)
#define VSetE(X)	(pt1 = (tagged_t *)(X))

/* -------------------------------------------------------------------
   FRAME/CHOICEPT MANIPULATIONS
   ----------------------------------------------------------------------*/

void pop_frame(Arg)
     Argdecl;
{
  CIAO_REGISTER tagged_t *pt1;

  VSetE(w->frame); {
    CIAO_REGISTER int arity;

    arity = FrameSizeToCount(FrameSize(w->next_insn)); {
      CIAO_REGISTER int i;

      for(i=0; i<arity; i++) X(i) = Y(i);
    }
  }
  w->local_top = E;
  w->frame = E->frame;
  w->next_insn = E->next_insn;
}

/* this assumes w->local_top has been computed! */
void push_frame(Arg,arity)
     Argdecl;
     int arity;
{
  CIAO_REGISTER tagged_t *pt1;
  CIAO_REGISTER int i;

  VSetE(w->local_top);
  E->next_insn = w->next_insn;
  E->frame = w->frame;
  w->frame = E;
  w->next_insn = &contcode[(1+LOffset)*arity];
  w->local_top = (frame_t *)Offset(E,EToY0+arity);
  for(i=0; i<arity; i++)
    Y(i) = X(i);
}


void pop_choicept(Arg)
     Argdecl;
{
  CIAO_REGISTER node_t *b = w->node;

  w->node = b = ChoiceCharOffset(b,-b->next_alt->node_offset);
  SetShadowregs(b);
}

void push_choicept(Arg,alt)
     Argdecl;
     try_node_t *alt;
{
  CIAO_REGISTER int n = alt->node_offset;
  CIAO_REGISTER tagged_t *b0 = (tagged_t *)w->node;
  CIAO_REGISTER node_t *b = ChoiceCharOffset(b0,n);

  ComputeA(w->local_top,w->node);
  w->node = b;
  NewShadowregs(w->global_top);

  b->trail_top = w->trail_top;
  SaveGtop(b,w->global_top);
  b->next_alt = alt;
  b->frame = w->frame;
  b->next_insn = w->next_insn;
  SaveLtop(b);
  n = OffsetToArity(n);
  while (n>0)
    ChoicePush(b0,X(--n));
  if (ChoiceYounger(ChoiceOffset(w->node,CHOICEPAD),w->trail_top))
    choice_overflow(Arg,CHOICEPAD);
}

#if defined(PARBACK)
bool_t nd_suspension_point(Arg)
     Argdecl;
{
  //Reinstall next_insn and Frame (Done by fail).
  //Reinstal argument register
  tagged_t t = w->node->term[0];
  definition_t * func = (definition_t *) CTagToPointer(t);
  int i;
  for (i = 0; i < func->arity; i++) DEREF(X(i),CTagToPointer(TagToPointer(t)+i+1));
  //Take choice point out
  Arg->next_insn = restart_point_insn;

  return TRUE;
}
#endif

/*
   Support for the builtin C-predicate atom_concat/3 (see term_support.c)
*/

bool_t nd_atom_concat(Arg)
     Argdecl;
{
  int i = GetSmall(X(3));
  char *s, *s1, *s2;

  w->node->term[3] += (1 << SmallShift);

  s2 = GetString(X(2));

  s = Atom_Buffer;

  s1 = s2 + i;
  strcpy(s, s1);
  Unify_constant(init_atom_check(Atom_Buffer),X(1));

  strcpy(s, s2);
  *(s+i) = '\0';
  Unify_constant(init_atom_check(Atom_Buffer),X(0));

  if (i == strlen(s2))
    pop_choicept(Arg);
  
  return TRUE;
}

#if defined(TABLING)
bool_t nd_fake_choicept(Arg)
     Argdecl;
{
  pop_choicept(Arg);
  return FALSE;
}
#endif


/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       CURRENT_ATOM/1
   -----------------------------------------------------------------------*/

bool_t current_atom(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  if (TagIsATM(X(0)))
    return TRUE;
  if (! IsVar(X(0)))
    MINOR_FAULT("current_atom/1: incorrect 1st arg");
#if defined(ATOMGC)
  { 
    /* There is an (improbable) case: the 0-th table entry is empty.
       Take it into account. */
    CIAO_REGISTER int size = SwitchSize(ciao_atoms) >> 1;
    CIAO_REGISTER int i = 0;
    while (i < size && (atmtab[i] == NULL))
      i++;
    X(1) = MakeSmall(i);  /* Yes, I am not considering an empty symbol table */
  }
#else
  X(1) = TaggedZero;
#endif
  push_choicept(Arg,address_nd_current_atom);
  return nd_current_atom(Arg);
}

bool_t nd_current_atom(Arg)
     Argdecl;
{
  CIAO_REGISTER int i = GetSmall(X(1));

#if defined(ATOMGC)
 /* 

    Atom GC may leave holes in the atom table.  Therefore: 
    
    1- The "following" valid entry is not necessarily the next one; 
    we may have to skip a number of empty atom entries.

    2- Stopping when the number of indices read reaches the current number of
    atoms is not right.  We have to use instead the size of the table. 

  */

  CIAO_REGISTER int size = SwitchSize(ciao_atoms) >> 1;

  /* Invariant: at entry, the current i points to a nonempty atom */
  Unify_constant(TagIndex(ATM,i),X(0));
  /* Go forward until the next non-empty atom; final stop when the end of
     the table has been reached.  */
  i++;
  while(i < size && (atmtab[i] == NULL))
    i++;
  
  if (i < size)                                  /* We got the next index */
    w->node->term[1] = MakeSmall(i);
  else 
    pop_choicept(Arg);
#else
    w->node->term[1] += (1 << SmallShift);
    Unify_constant(TagIndex(ATM,i),X(0));
    
    if (i+1 == ciao_atoms->count)
      pop_choicept(Arg);
#endif

  return TRUE;
}


/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       $CURRENT_CLAUSES/2
   -----------------------------------------------------------------------*/

/* This used to be nondeterministic. */
/* ASSERT: X1 is always unbound, unconditional. */
bool_t current_clauses(Arg)
     Argdecl;
{
  tagged_t *junk;
  definition_t *d;

  DEREF(X(0),X(0));
  if (! IsVar(X(0))) {
    d = find_definition(predicates_location,X(0),&junk,FALSE);
    if ((d!=NULL) && (d->predtyp==ENTER_INTERPRETED)) {
      return (CTagToPointer(X(1))=PointerToTerm(d->code.intinfo), TRUE);
    } else {
      return FALSE;
    }
  }
  else {
    MINOR_FAULT("$current_clauses/2: incorrect 1st arg");
  }
}


/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       REPEAT/0
   -----------------------------------------------------------------------*/

bool_t prolog_repeat(Arg)
     Argdecl;
{
  push_choicept(Arg,address_nd_repeat);
  return TRUE;
}

bool_t nd_repeat(Arg)
     Argdecl;
{
  return TRUE;
}


/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       CURRENT_PREDICATE/2
   -----------------------------------------------------------------------*/

bool_t current_predicate(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  if (!IsVar(X(1))) {
    tagged_t *junk;
    definition_t *d =
      find_definition(predicates_location,X(1),&junk,FALSE);

    if (d==NULL || d->predtyp==ENTER_UNDEFINED /*  || d->properties.public */)
      return FALSE;
    Unify_constant(d->printname,X(0));
    return TRUE;
  }

  X(2) = MakeSmall(SwitchSize(*predicates_location));
  X(3) = PointerToTerm(*predicates_location);
  push_choicept(Arg,address_nd_current_predicate);
  return nd_current_predicate(Arg);
}

bool_t nd_current_predicate(Arg)
     Argdecl;
{
  sw_on_key_t *table = (sw_on_key_t *)TermToPointer(X(3));
  CIAO_REGISTER sw_on_key_node_t *keyval;
  CIAO_REGISTER int j = GetSmall(X(2));
  CIAO_REGISTER definition_t *d;
  tagged_t mask = (IsVar(X(0)) ? 0 : INDEXMASK);

  for (--j; j>=0; --j) {
    keyval = &table->tab.asnode[j];
    if ((keyval->key & mask) == (X(0) & mask) &&
        (d = keyval->value.def) &&
        d->predtyp != ENTER_UNDEFINED /*  && !d->properties.public */ ){
      if (j==0)
        pop_choicept(Arg);
      else
        w->node->term[2] = MakeSmall(j);
      Unify_constant(d->printname,X(0));
      return cunify(Arg,
                    make_structure(Arg,SetArity(d->printname,d->arity)),
                    X(1));
    }
  }
  pop_choicept(Arg);
  return FALSE;
}



/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       PREDICATE_PROPERTY/2
   -----------------------------------------------------------------------*/

 /* See dummy.pl (MCL) */

static unsigned int predicate_property_bits(d)
     CIAO_REGISTER definition_t *d;
{
  return
    /* (d->properties.public     ?  0x1 : 0) | */
      (d->properties.concurrent ?  0x1 : 0)
    | (d->properties.dynamic    ?  0x2 : 0)
    | (d->properties.wait       ?  0x4 : 0)
    | (d->properties.multifile  ?  0x8 : 0)
    ;
}



bool_t predicate_property(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  if (! IsVar(X(0))) {
    tagged_t *junk;
    definition_t *d =
      find_definition(predicates_location,X(0),&junk,FALSE);

    if (d==NULL || d->predtyp==ENTER_UNDEFINED)
      return FALSE;
    Unify_constant(MakeSmall(d->predtyp),X(1));
    Unify_constant(MakeSmall(predicate_property_bits(d)),X(2));
    return TRUE;
  }

  X(3) = MakeSmall(SwitchSize(*predicates_location));
  X(4) = PointerToTerm(*predicates_location);
  push_choicept(Arg,address_nd_predicate_property);
  return nd_predicate_property(Arg);
}

bool_t nd_predicate_property(Arg)
     Argdecl;
{
  sw_on_key_t *table = (sw_on_key_t *)TermToPointer(X(4));
  CIAO_REGISTER int j = GetSmall(X(3));
  CIAO_REGISTER sw_on_key_node_t *keyval;
  CIAO_REGISTER definition_t *d;

  for (--j; j>=0; --j)
    {
      keyval = &table->tab.asnode[j];
      if ((d = keyval->value.def) &&
	  d->predtyp != ENTER_UNDEFINED)
	{
	  if (j==0)
	    pop_choicept(Arg);
	  else
	    w->node->term[3] = MakeSmall(j);
	  Unify_constant(MakeSmall(d->predtyp),X(1));
	  Unify_constant(MakeSmall(predicate_property_bits(d)),X(2));
	  return cunify(Arg,make_structure(Arg,SetArity(d->printname,d->arity)),
			X(0));
	}
    }
  pop_choicept(Arg);
  return FALSE;
}


/* ------------------------------------------------------------------
   THE BUILTIN C-PREDICATE       $CURRENT_INSTANCE/5
   -----------------------------------------------------------------------*/

/* ASSERT: X(2) is a dereferenced small integer */

/* MCL: a typical call is '$current_instance'(Head, Body, Root, Ptr,
   Blocking) where Head is the head clause, Body is the body clause ("true"
   for facts), Root is a small integer denoting the root of the predicate
   (obtained through '$current_clauses'/2), and Ptr is a number denoting the
   particular clause (i.e., a pointer to the instance_t). "Blocking"
   specifies whether the predicate must block or not on concurrent
   predicates. */

instance_t *current_instance(Arg)
     Argdecl;
{
  int_info_t *root;
  BlockingType block;
#if defined(PROFILE)
  tagged_t *junk;
  w->node->functor=find_definition(predicates_location,X(0),&junk,FALSE);
#endif
  PredTrace("I",w->node->functor);
  root =  TagToRoot(X(2));
  if (root->behavior_on_failure == DYNAMIC)
    return current_instance_noconc(Arg);
  else {
    DEREF(X(4), X(4));                                   /* Blocking? (MCL) */
#if defined(DEBUG)
    if (X(4) == atom_block)
      block = BLOCK;
    else if (X(4) == atom_no_block)
      block = NO_BLOCK;
    else {
      failc("$current_instance called with unkown 5th argument");
      return NULL;                         
    }
#else
    block = X(4) == atom_block ? BLOCK : NO_BLOCK;
#endif
    return current_instance_conc(Arg, block);
  }
}


 /* Take time into account, do not wait for predicates. */

static instance_t *current_instance_noconc(Arg)
     Argdecl;
{
  CIAO_REGISTER instance_t *x2_chain, *x5_chain;
  CIAO_REGISTER instance_t *x2_next=NULL, *x5_next=NULL;
  CIAO_REGISTER int_info_t *root = TagToRoot(X(2));
  CIAO_REGISTER tagged_t head;


  Wait_Acquire_Cond_lock(root->clause_insertion_cond);
  head=X(0); 
  DerefSwitch(head,X(0),goto var_case_switch;);
  if (TagIsSTR(head)) {
    DerefArg(head,head,1);
    if (IsVar(head)) {
    var_case_switch:
      x5_chain = x2_chain = ACTIVE_INSTANCE(Arg,root->first,use_clock,TRUE);
      if (x2_chain)
        x5_next = x2_next =
          ACTIVE_INSTANCE(Arg,x2_chain->forward,use_clock,TRUE);
      else {
        Release_Cond_lock(root->clause_insertion_cond);
        return NULL;
      }
    }
    else if (TagIsLST(head)){
      x5_chain = ACTIVE_INSTANCE(Arg,root->lstcase,use_clock,FALSE);
    xn_switch:
      x2_chain = ACTIVE_INSTANCE(Arg,root->varcase,use_clock,FALSE);

      if (x2_chain && x5_chain) {
        if (x2_chain->rank < x5_chain->rank){
          x2_next =
            ACTIVE_INSTANCE(Arg,x2_chain->next_forward,use_clock,FALSE);
          x5_next = x5_chain;
          x5_chain = NULL;
        } else {
          x5_next =
            ACTIVE_INSTANCE(Arg,x5_chain->next_forward,use_clock,FALSE);
          x2_next = x2_chain;
          x2_chain = NULL;
        }
      } else if (x2_chain)
        x2_next = ACTIVE_INSTANCE(Arg,x2_chain->next_forward,use_clock,FALSE);
      else if (x5_chain)
        x5_next = ACTIVE_INSTANCE(Arg,x5_chain->next_forward,use_clock,FALSE);
      else {
        Release_Cond_lock(root->clause_insertion_cond);
        return NULL;                                    /* No solution */
      }
    } else {
      sw_on_key_node_t *hnode;

      if (TagIsSTR(head))
        hnode = incore_gethash(root->indexer,TagToHeadfunctor(head));
      else
        hnode = incore_gethash(root->indexer,head);
	
      x5_chain = ACTIVE_INSTANCE(Arg,hnode->value.instp,use_clock,FALSE);
      goto xn_switch;
    }
  }
  else goto var_case_switch;

  if (x2_next || x5_next) {
    ComputeA(w->local_top,w->node);
    X(X2_CHN) = PointerToTermOrZero(x2_next);
    X(ClockSlot) = MakeSmall(use_clock);
    X(X5_CHN) = PointerToTermOrZero(x5_next);
    X(RootArg) = PointerToTermOrZero(root);
    /* Clean unused registers (JF & MCL) */
    X(InvocationAttr) = MakeSmall(0); 
    X(PrevDynChpt) = MakeSmall(0); 

    w->next_alt = address_nd_current_instance; /* establish skeletal node */
    w->next_node = w->node;
    w->node = ChoiceCharOffset(w->node,ArityToOffset(DynamicPreserved));
    w->node->next_alt = NULL;
    w->node->trail_top = w->trail_top;
    SaveGtop(w->node,w->global_top);
    NewShadowregs(w->global_top);
  }

  Release_Cond_lock(root->clause_insertion_cond);

  if (!x2_chain)
    return x5_chain;
  else
    return x2_chain;
}


/* First-solution special case of the above. */
/* ASSERT: X(0) is a dereferenced small integer,
           X(1) is a dereferenced unconditional unbound */
/* Adapted to concurrent predicates (MCL) */
bool_t first_instance(Arg)
     Argdecl;
{
  CIAO_REGISTER int_info_t *root = TagToRoot(X(0));
  CIAO_REGISTER instance_t *inst;

  if (root->behavior_on_failure == DYNAMIC) 
    inst = ACTIVE_INSTANCE(Arg,root->first,use_clock,TRUE);
  else {
    Cond_Begin(root->clause_insertion_cond);
    inst = root->first;
    Broadcast_Cond(root->clause_insertion_cond);
  }

  if (!inst)
    return FALSE;		                          /* no solutions */
  CTagToPointer(X(1)) = PointerToTerm(inst);

  return TRUE;
}

/* ------------------------------------------------------------------
	  NEXT_INSTANCE
   -----------------------------------------------------------------------*/

bool_t next_instance(Arg,ipp)
    Argdecl;
    instance_t **ipp;
{
    CIAO_REG_1(instance_t *, x2_insp);
    CIAO_REG_2(instance_t *, x5_insp);
    CIAO_REG_3(int_info_t *,  root);
    instance_clock_t clock = GetSmall(X(4));

    x2_insp = *ipp =  TagToInstance(X(2));
    x5_insp = TagToInstance(X(5));
    root = TagToRoot(X(6));


    Wait_Acquire_Cond_lock(root->clause_insertion_cond);

    if (x2_insp == x5_insp)
	x2_insp = x5_insp = ACTIVE_INSTANCE(Arg,x2_insp->forward,clock,TRUE);
    else if (!x2_insp)
    {
    x5_alt:
	*ipp = x5_insp;
	x5_insp = ACTIVE_INSTANCE(Arg,x5_insp->next_forward,clock,FALSE);
    }
    else if (!x5_insp)
    x2_alt:
	x2_insp = ACTIVE_INSTANCE(Arg,x2_insp->next_forward,clock,FALSE);

    else if (x2_insp->rank < x5_insp->rank)
	goto x2_alt;
    else
	goto x5_alt;

    Release_Cond_lock(root->clause_insertion_cond);

    if (!x2_insp && !x5_insp)
	return FALSE;
    else {
      w->node->term[X2_CHN] = X(X2_CHN) = PointerToTermOrZero(x2_insp);
      w->node->term[X5_CHN] = X(X5_CHN) = PointerToTermOrZero(x5_insp);
      return  TRUE;
    }
}


#if defined(OLD_DATABASE)
/* ------------------------------------------------------------------
	$CURRENT_KEY/4
   -----------------------------------------------------------------------*/

bool_t current_key(Arg)
    Argdecl;
{
  int_info_t *root = TagToRoot(X(0));
  CIAO_REGISTER sw_on_key_t *swp = root->indexer;
  CIAO_REGISTER int j = SwitchSize(swp);
  tagged_t mask;

  extern tagged_t decode_instance_key PROTO((instance_t *));

  DEREF(X(2),X(2));
  mask = (IsVar(X(2)) ? 0 : INDEXMASK);
  DEREF(X(3),X(3));
  X(4) = atom_nil;

  if (!IsVar(X(3))){
    if (TagIsLST(X(3))) {
      if (ACTIVE_INSTANCE(Arg,root->lstcase,use_clock,FALSE))
        MakeLST(X(4),make_structure(Arg,functor_list),X(4));
    } else if (TagIsSTR(X(3))) {
      CIAO_REGISTER sw_on_key_node_t *hnode =
        incore_gethash(swp,TagToHeadfunctor(X(3)));
      CIAO_REGISTER instance_t *inst =
        ACTIVE_INSTANCE(Arg,hnode->value.instp,use_clock,FALSE);

      if (inst && !(hnode->key & QMask)) {
        MakeLST(X(4),make_structure(Arg,hnode->key),X(4));
      } else
        while (inst){
          int ar = LargeArity(hnode->key);

          if (HeapDifference(w->global_top,Heap_End)<CONTPAD+ar+3)
            explicit_heap_overflow(Arg,SOFT_HEAPPAD+ar,5);

          MakeLST(X(4),decode_instance_key(inst),X(4));
          inst = ACTIVE_INSTANCE(Arg,inst->next_forward,use_clock,FALSE);
        }
    } else {
      CIAO_REGISTER sw_on_key_node_t *hnode = incore_gethash(swp,X(3));
      CIAO_REGISTER instance_t *inst =
        ACTIVE_INSTANCE(Arg,hnode->value.instp,use_clock,FALSE);

      if (inst)
        MakeLST(X(4),make_structure(Arg,hnode->key),X(4));
    }
    CTagToPointer(X(1)) = X(4);
    return TRUE;
  }

  if (ACTIVE_INSTANCE(Arg,root->lstcase,use_clock,FALSE) &&
      (functor_list & mask) == (X(2) & mask))
    MakeLST(X(4),make_structure(Arg,functor_list),X(4));
  for (--j; j>=0; --j) {
    CIAO_REGISTER sw_on_key_node_t *hnode = &swp->tab.asnode[j];
    CIAO_REGISTER instance_t *inst =
      ACTIVE_INSTANCE(Arg,hnode->value.instp,use_clock,FALSE);

    if (!(hnode->key & QMask)){
      if (inst && (hnode->key & mask) == (X(2) & mask)) {
        if (HeapDifference(w->global_top,Heap_End)<CONTPAD+ARITYLIMIT+3)
          explicit_heap_overflow(Arg,SOFT_HEAPPAD,5);

        MakeLST(X(4),make_structure(Arg,hnode->key),X(4));
      }
    } else {
      if (IsVar(X(2)) ||
          (TagIsSTR(X(2)) && hnode->key==TagToHeadfunctor(X(2))))
        while (inst){
          int ar = LargeArity(hnode->key);

          if (HeapDifference(w->global_top,Heap_End)<CONTPAD+ar+3)
            explicit_heap_overflow(Arg,SOFT_HEAPPAD+ar,5);

          MakeLST(X(4),decode_instance_key(inst),X(4));
          inst = ACTIVE_INSTANCE(Arg,inst->next_forward,use_clock,FALSE);
        }
    }
  }
  CTagToPointer(X(1)) = X(4);
  return TRUE;
}
#endif

#if defined(THREADS)
bool_t close_predicate(Arg)
     Argdecl;
{
  CIAO_REGISTER int_info_t *root = TagToRoot(X(0));

  Cond_Begin(root->clause_insertion_cond);
  if (root->behavior_on_failure == CONC_OPEN) 
    root->behavior_on_failure = CONC_CLOSED;
  Broadcast_Cond(root->clause_insertion_cond);

  return TRUE;
}

bool_t open_predicate(Arg)
     Argdecl;
{
  CIAO_REGISTER int_info_t *root = TagToRoot(X(0));


  Cond_Begin(root->clause_insertion_cond);
  if (root->behavior_on_failure == CONC_CLOSED) 
    root->behavior_on_failure = CONC_OPEN;
  Broadcast_Cond(root->clause_insertion_cond);

  return TRUE;
}
#else                                                          /* !THREADS */
bool_t close_predicate(Arg)
     Argdecl;
{
  return TRUE;
}

bool_t open_predicate(Arg)
     Argdecl;
{
  return TRUE;
}
#endif


/* Similar to current_instance for concurrent predicates: wait if no clauses
   match and the predicate is not closed; also, handle the cases of
   predicate retraction, assertion, etc. by maintaining and moving around a
   list with the calls pending on every clause of a predicate.  When the end
   of the list is reached and the predicate is still "open" (i.e., more
   clauses might be added to it), the call does not fail: it is instead
   enqueued in a list reacheable from the root of the predicate. (MCL) 

   The case for non-blocking calls is interesting: there are three
   possibilities:

   a) There is no clause for the predicate
   b) There are clauses, but indexing dictates that none of them are 
      applicable for the current call.
   c) There are clauses, and at least one of them can match the current call.

   In cases (a) and (b) a non-blocking call does *not* need choicepoint.
   Case (c) needs a choicepoint, and the clause remains locked while it is
   being executed.  The lock is removed when execution finishes, or when the
   choicepoint is deallocated.

*/

static instance_t *current_instance_conc(Arg, block)
     Argdecl;
     BlockingType block;
{
  instance_t *x2_n = NULL, *x5_n = NULL;
  instance_t *current_one;
  bool_t try_instance;
  instance_handle_t *x2_next, *x5_next;
  CIAO_REGISTER int_info_t *root = TagToRoot(X(2));

#if defined(DEBUG)
  if (debug_concchoicepoints) 
    fprintf(stderr, 
"** Entering current_instance_conc, node = %x, next_node = %x, conc. = %x\n",
            (int)w->node, (int)w->next_node, (int)TopConcChpt
            );
#endif

  do {
    try_instance =
     wait_for_an_instance_pointer(&(root->first), &(root->first), root, block);

 /* That was a non-blocking or concurrent predicate with no instance at all */
    if (!try_instance) {
      Wait_For_Cond_End(root->clause_insertion_cond);
#if defined(DEBUG)
    if (debug_concchoicepoints)
      fprintf(stderr,
"***(%d)(%d) Exiting current_instance_conc with failure, node = %x, next_node = %x, conc. node = %x\n",
              (int)Thread_Id, (int)GET_INC_COUNTER,
              (int)w->node, (int)w->next_node, (int)TopConcChpt);
#endif
      return NULL;
    }

/* If we are here, we have a lock for the predicate.  Get first possibly
   matching instance */

    current_one = first_possible_instance(X(0), root, &x2_n, &x5_n);
    if (current_one == NULL) { /* Let others enter and update */
        Wait_For_Cond_End(root->clause_insertion_cond);
    }
  } while (!current_one && block == BLOCK);

  /* Here with (current_one || block == NON_BLOCK).  current_one == NULL
     implies that we do not have a lock --- but then block == NON_BLOCK, and
     therefore we can return with a NULL */
     
  if (!current_one) {
#if defined(DEBUG)
    if (debug_concchoicepoints)
      fprintf(stderr,
"***(%d)(%d) Exiting current_instance_conc with failure, node = %x, next_node = %x, conc. node = %x\n",
              (int)Thread_Id, (int)GET_INC_COUNTER,
              (int)w->node, (int)w->next_node, (int)TopConcChpt);
#endif
    return NULL;
  }

#if defined(DEBUG) && defined(THREADS)
  if (debug_concchoicepoints && 
      Cond_Lock_is_unset(root->clause_insertion_cond))
      fprintf(stderr, 
              "***%d(%d) current_instance_conc: putting chpt without locks!\n",
            (int)Thread_Id, (int)GET_INC_COUNTER);
  if (debug_conc && !root->first)
    fprintf(stderr,"*** current_instance_conc: no first instance!\n");
#endif

 /* We do NOT release the clause lock here: we must retain it until we
    finish executing it.  It is released at Prolog level.  The same
    for next instance: when it provides a possible solution, it leaves
    a lock set, to be unset from Prolog.  If the Prolog unification
    fails, the lock is anyway unset from wam(), right before
    failure. */

#if defined(DEBUG) && defined(THREADS)
  if (debug_concchoicepoints)
    fprintf(stderr,
            "*** %d(%d) in c_i making chpt (now: node = %x)., inst. is %x\n",
            (int)Thread_Id, (int)GET_INC_COUNTER, 
            (int)w->node, (int)current_one);
#endif

    x2_next = make_handle_to(x2_n, root, X(0), X2);
    x5_next = make_handle_to(x5_n, root, X(0), X5);
    ComputeA(w->local_top,w->node);
    X(X2_CHN) = PointerToTermOrZero(x2_next);
    X(ClockSlot) = MakeSmall(use_clock);
    X(X5_CHN) = PointerToTermOrZero(x5_next);

    /* pass root to RETRY_INSTANCE (MCL) */
    X(RootArg) = PointerToTermOrZero(root);  
    if (block == BLOCK)
      SET_BLOCKING(X(InvocationAttr));
    else
      SET_NONBLOCKING(X(InvocationAttr));
    SET_EXECUTING(X(InvocationAttr));

    /* Save last dynamic top */
    X(PrevDynChpt) = PointerToTermOrZero(TopConcChpt); 
    w->next_alt = address_nd_current_instance; /* establish skeletal node */
    w->next_node = w->node;
    w->node = ChoiceCharOffset(w->node,ArityToOffset(DynamicPreserved));
    TopConcChpt = (node_t *)w->node;  /* Update dynamic top */
    w->node->next_alt = NULL;
    w->node->trail_top = w->trail_top;
    SaveGtop(w->node,w->global_top);
    NewShadowregs(w->global_top);

#if defined(DEBUG)
    if (debug_concchoicepoints)
      fprintf(stderr,
"***(%d)(%d) Exiting current_instance_conc, node = %x, next_node = %x, conc. node = %x\n",
              (int)Thread_Id, (int)GET_INC_COUNTER,
              (int)w->node, (int)w->next_node, (int)TopConcChpt);
#endif

  return current_one;
}



static bool_t wait_for_an_instance_pointer(inst_pptr1, inst_pptr2, root, block)
     instance_t **inst_pptr1, **inst_pptr2;
     int_info_t *root;
     BlockingType block;
{

  volatile instance_t *pptr1 = NULL, *pptr2 = NULL;

   /* We have to wait for a new clause only if we are blocking */

    while(TRUE){  
    /* Wait until a change is signaled, and test that the change affects us */

      if (block == BLOCK) {
        Wait_For_Cond_Begin( \
                             ((*inst_pptr1 == NULL) && \
                              (*inst_pptr2 == NULL) && \
                              root->behavior_on_failure == CONC_OPEN ), \
                              root->clause_insertion_cond \
                             )
      } else { /* In any case, leave the predicate locked */
        Cond_Begin(root->clause_insertion_cond);
      }
          
      /* Test again to find out which was the case */
      
    pptr1 = *inst_pptr1;
    pptr2 = *inst_pptr2;
    if (pptr1 || pptr2) 
      return TRUE;
    else if (block == NO_BLOCK || root->behavior_on_failure == CONC_CLOSED)
      return FALSE;
    else Wait_For_Cond_End(root->clause_insertion_cond); /*Let others update*/
    }
}

static instance_t *first_possible_instance(x0, root, x2_n, x5_n)
     tagged_t x0;
     int_info_t *root;
     instance_t **x2_n, **x5_n;
{
  CIAO_REGISTER instance_t *x2_chain, *x5_chain;
  CIAO_REGISTER instance_t *x2_next, *x5_next;
  sw_on_key_node_t *hnode;
  tagged_t head;

  head = x0;
  DerefSwitch(head,x0,goto var_case_switch;);

  x2_next = x5_next = NULL;
  if (TagIsSTR(head)) {
    DerefArg(head,head,1);
    if (IsVar(head)) {
    var_case_switch:
      x5_chain = x2_chain = root->first;                 /* normal = TRUE */
      if (x2_chain)
        x5_next = x2_next = x2_chain->forward;           /* normal = TRUE */
      else return NULL;
    }
    else if (TagIsLST(head)){
      x5_chain = root->lstcase;                         /* normal = FALSE */
    xn_switch:
      x2_chain = root->varcase;                         /* normal = FALSE */
       if (x2_chain && x5_chain) {
        if (x2_chain->rank < x5_chain->rank){
          x2_next = x2_chain->next_forward;             /* normal = FALSE */
          x5_next = x5_chain;
          x5_chain = NULL;
        } else {
          x5_next = x5_chain->next_forward;             /* normal = FALSE */
          x2_next = x2_chain;
          x2_chain = NULL;
        }
      } else if (x2_chain)
        x2_next = x2_chain->next_forward;               /* normal = FALSE */
      else if (x5_chain)
        x5_next = x5_chain->next_forward;               /* normal = FALSE */
      else return NULL;                                    /* No solution */
    } else {
      hnode = TagIsSTR(head) ?
              incore_gethash(root->indexer,TagToHeadfunctor(head)) :
              incore_gethash(root->indexer,head);
      x5_chain = hnode->value.instp;                    /* normal = FALSE */
      goto xn_switch;
    }
  }
  else goto var_case_switch;

  *x2_n  = x2_next;
  *x5_n  = x5_next;

  return x2_chain ? x2_chain : x5_chain;
}


bool_t next_instance_conc(Arg,ipp)
    Argdecl;
    instance_t **ipp;
{
  CIAO_REGISTER int_info_t *root = TagToRoot(X(RootArg));
  BlockingType block;      
  instance_handle_t *x2_ins_h, *x5_ins_h;
  bool_t next_instance_pointer;
  instance_t *x2_insp, *x5_insp;

#if defined(DEBUG)
    if (debug_concchoicepoints)
      fprintf(stderr,
"*** Entering next_instance_conc, node = %x, next_node = %x, conc. node = %x\n",
              (int)w->node, (int)w->next_node, (int)TopConcChpt
);
#endif

  /* = X(7) == atom_block ? BLOCK : NO_BLOCK;*/
  block = IS_BLOCKING(X(InvocationAttr)) ? BLOCK : NO_BLOCK; 

  /* When we baktrack after a call which did not finally succeed, the lock
     is still set. Unlock it before proceeding to the next clause. */

  if (EXECUTING(X(InvocationAttr))){
#if defined(DEBUG)
    if (debug_concchoicepoints)
      fprintf(stderr, "*** in next_instance_conc changing to nonexecuting\n");
#endif
    SET_NONEXECUTING(X(InvocationAttr));
    Wait_For_Cond_End(root->clause_insertion_cond);
  }
  
  x2_ins_h = TagToInstHandle(X(X2_CHN));
  x5_ins_h = TagToInstHandle(X(X5_CHN));

/* x2_ins_h->inst_ptr and x5_ins_h->inst_ptr may be both NULL; that means no
   current instance is available.  Just wait for one.  If any of x2_insp or
   x5_insp are NULL pointer, they are automatically enqueued in root by
   change_handle_to_instance */

#if defined(DEBUG) && defined(THREADS)
  if (debug_conc && !root->first)
    fprintf(stderr, 
            "*** %d(%d) in next_instance_conc without first instance.\n",
            (int)Thread_Id, (int)GET_INC_COUNTER);
  if (debug_conc) {
    fprintf(stderr,
      "*** %d(%d) in next_instance_conc with x2 = %x, x5 = %x, block = %x\n",
            (int)Thread_Id, (int)GET_INC_COUNTER, 
            (int)x2_ins_h->inst_ptr, (int)x5_ins_h->inst_ptr, (int)block);
  }
#endif

  do {
    next_instance_pointer =
      wait_for_an_instance_pointer(&(x2_ins_h->inst_ptr), 
                                   &(x5_ins_h->inst_ptr),
                                   root, block);
    if (!next_instance_pointer) {           /* Closed or non-waiting call */
      remove_handle(x2_ins_h, root, X2);
      remove_handle(x5_ins_h, root, X5);
      *ipp = NULL;                                       /* Cause failure */
      /* Time for new assertions */
      Wait_For_Cond_End(root->clause_insertion_cond);
#if defined(DEBUG)
    if (debug_concchoicepoints)
      fprintf(stderr,
"***(%d)(%d) Exiting current_instance_conc with failure, node = %x, next_node = %x, conc. node = %x\n",
              (int)Thread_Id, (int)GET_INC_COUNTER,
              (int)w->node, (int)w->next_node, (int)TopConcChpt);
#endif
      return FALSE;                                 /* Remove choicepoint */
    }

    /* Locate a satisfactory instance. */
    jump_to_next_instance(x2_ins_h->inst_ptr, x5_ins_h->inst_ptr,
                          ipp, &x2_insp, &x5_insp);

    /* Move handle forwards to re-start (if necesary) in a new clause */
    change_handle_to_instance(x2_ins_h, x2_insp, root, X2);
    change_handle_to_instance(x5_ins_h, x5_insp, root, X5);

#if defined(DEBUG) && defined(THREADS)
    if (debug_conc && !root->first)
      fprintf(stderr, 
              "*** %d(%d) after jumping without first instance.\n",
              (int)Thread_Id, (int)GET_INC_COUNTER);
#endif

    if (!*ipp) /* Not instance -> release lock, continue in loop */
      Wait_For_Cond_End(root->clause_insertion_cond);
  } while (!*ipp);

#if defined(DEBUG) && defined(THREADS)
  if (debug_conc && !root->first)
    fprintf(stderr, 
            "*** %d(%d) exiting n_i without first instance.\n",
            (int)Thread_Id, (int)GET_INC_COUNTER);
  if (debug_conc)
    fprintf(stderr, 
            "*** %d(%d) exiting n_i with instance %x.\n",
            (int)Thread_Id, (int)GET_INC_COUNTER, (int)*ipp);
#endif  

  /* Here with a possibly matching instance,
     a possibly empty next instance,
     and the lock on the instance. */

  w->node->term[X2_CHN] = X(X2_CHN) = PointerToTermOrZero(x2_ins_h);
  w->node->term[X5_CHN] = X(X5_CHN) = PointerToTermOrZero(x5_ins_h);
  SET_EXECUTING(X(InvocationAttr));
#if defined(DEBUG)
    if (debug_concchoicepoints)
      fprintf(stderr,
"***(%d)(%d) Exiting current_instance_conc, node = %x, next_node = %x, conc. node = %x\n",
              (int)Thread_Id, (int)GET_INC_COUNTER,
              (int)w->node, (int)w->next_node, (int)TopConcChpt);
#endif

  return TRUE;
}


 /* Current pointers to instances are x2_insp and x5_insp; look for a new
    pointer which presumably matches the query. */

void jump_to_next_instance(x2_insp, x5_insp,
                           ipp, x2_next, x5_next)
    instance_t *x2_insp, *x5_insp, **ipp, **x2_next, **x5_next;
{
  *ipp = *x2_next = x2_insp;
  *x5_next = x5_insp;

  if (!x2_insp && !x5_insp)                                        /* MCL */
    return;

  if (x2_insp == x5_insp)
    x2_insp = x5_insp = x2_insp->forward;               /* normal = TRUE */
  else if (!x2_insp) {
  x5_alt:
    *ipp = x5_insp;
    x5_insp = x5_insp->next_forward;                    /* normal = FALSE */
  } else if (!x5_insp)
    x2_alt:
  x2_insp = x2_insp->next_forward;                      /* normal = FALSE */
  else if (x2_insp->rank < x5_insp->rank)
    goto x2_alt;
  else
    goto x5_alt;

  *x2_next = x2_insp;
  *x5_next = x5_insp;
}


/****************************************************************************/

/* Add an invocation as pending from an instance; if there is anyone else
   pending on that instance, add ourselves to the list. */

instance_handle_t *make_handle_to(inst, root, head, chain)
     instance_t *inst;                            /* Pending to here */
     int_info_t *root;                             /* Predicate root */
     tagged_t head;
     WhichChain chain;                               /* Is that X2 or X5? */
{
  instance_handle_t *this_handle;
                                                     /* Create the handle */
  this_handle = (instance_handle_t *)checkalloc(sizeof(instance_handle_t));

  this_handle->head = head;                        /* Allow re-indexation */
  link_handle(this_handle, inst, root, chain);
#if defined(DEBUG) && defined(THREADS)
  if (debug_conc)
    fprintf(stderr, 
            "*** %d(%d) made handle %x to instance %x\n", 
            (int)Thread_Id, (int)GET_INC_COUNTER, (int)this_handle, (int)inst);
#endif

  return this_handle;
}


/* Remove handle from list.  xi might be pointed to directly from the root,
   or from an instance record: need to update the pointer itself. */

void remove_handle(xi, root, chain)
     instance_handle_t *xi;
     int_info_t *root;
     WhichChain chain;
{
  unlink_handle(xi, root, chain);
  checkdealloc((tagged_t *)xi, sizeof(instance_handle_t));

#if defined(DEBUG) && defined(THREADS)
  if (debug_conc)
    fprintf(stderr, "*** %d(%d) removed handle %x (to instance %x)\n", 
            (int)Thread_Id, (int)GET_INC_COUNTER, (int)xi, (int)xi->inst_ptr);
#endif

}



/* Make a handle to point to a new instance. */

static void change_handle_to_instance(handle, new_inst, root, chain)
     instance_handle_t *handle;
     instance_t *new_inst;
     int_info_t *root;
     WhichChain chain;
{
  if (handle->inst_ptr != new_inst) {      /* Do not move if not necessary */
#if defined(DEBUG)
    if (debug_conc)
      fprintf(stderr, 
              "*** %d(%d) changes handle %x from instance %x to %x\n", 
              (int)Thread_Id, (int)GET_INC_COUNTER, (int)handle,
              (int)handle->inst_ptr, (int)new_inst);
#endif
    unlink_handle(handle, root, chain);
    link_handle(handle, new_inst, root, chain);
  }
}


static void link_handle(handle, inst, root, chain)
     instance_handle_t *handle;
     instance_t *inst;                            /* Pending to here */
     int_info_t *root;                             /* Predicate root */
     WhichChain chain;                               /* Is that X2 or X5? */
{
#if defined(DEBUG) 
  if (debug_conc && Cond_Lock_is_unset(root->clause_insertion_cond))
    fprintf(stderr, "*** Thread %d(%d) in link_handle() with lock unset!\n",
            (int)Thread_Id, (int)GET_INC_COUNTER);
#endif

  handle->inst_ptr = inst;             /* Instance we are looking at */
  handle->previous_handle = NULL;
  if (inst) {           /* Non-null instances go either to X2 or to X5... */
    if (chain == X2) {
      handle->next_handle = inst->pending_x2;
      inst->pending_x2 = handle;
    } else {
      handle->next_handle = inst->pending_x5;
      inst->pending_x5 = handle;
    }
  } else {                    /* handles to NULL instances go to the root */
    if (chain == X2) {
      handle->next_handle = root->x2_pending_on_instance;
      root->x2_pending_on_instance = handle;
    } else {
      handle->next_handle = root->x5_pending_on_instance;
      root->x5_pending_on_instance = handle;
    }
  }
  if (handle->next_handle)
    handle->next_handle->previous_handle = handle;
}


static void unlink_handle(xi, root, chain)
     instance_handle_t *xi;
     int_info_t *root;
     WhichChain chain;
{
  instance_t *inst;

#if defined(DEBUG)
  if (debug_conc && Cond_Lock_is_unset(root->clause_insertion_cond))
    fprintf(stderr, "*** Thread_Id %d(%d) in unlink_handle() with lock unset!\n", 
            (int)Thread_Id, (int)GET_INC_COUNTER);
#endif

/* A handle is enqueued in X2 (unindexed links) or X5 (indexed links) iff it
   has a non-null instance pointer; otherwise, it must be enqueued in the
   root queue. */

  if ((inst = xi->inst_ptr)) {

    if (chain == X2 && inst->pending_x2 == xi)          /* First in queue */
      inst->pending_x2 = xi->next_handle;

    if (chain == X5 && inst->pending_x5 == xi)
      inst->pending_x5 = xi->next_handle;

  } else if (chain == X2) {             /* xi->inst_ptr is a NULL pointer */
    if (root->x2_pending_on_instance == xi)
      root->x2_pending_on_instance = xi->next_handle;
  } else {
    if (root->x5_pending_on_instance == xi)
      root->x5_pending_on_instance = xi->next_handle;
  }

  if (xi->next_handle)
    xi->next_handle->previous_handle = xi->previous_handle;
  if (xi->previous_handle)
    xi->previous_handle->next_handle = xi->next_handle;
}

 /* Move all elements of a queue to another queue, and make all of them to
    point to the instance destinst */

void move_queue(srcq, destq, destinst)
     instance_handle_t **srcq;
     instance_handle_t **destq;
     instance_t *destinst;
{
  instance_handle_t *last, *running = *srcq;

#if defined(DEBUG) && defined(THREADS)
  int_info_t *root =
    *srcq && (*srcq)->inst_ptr ? (*srcq)->inst_ptr->root : NULL;
  int counter = 0;

  if (debug_conc && root && Cond_Lock_is_unset(root->clause_insertion_cond))
    fprintf(stderr, "*** in move_queue() with lock unset!\n");

  if (debug_conc)
    fprintf(stderr,
            "*** %d(%d) moving queue from %x to %x (-> instance %x)\n",
            (int)Thread_Id, (int)GET_INC_COUNTER, 
            (int)srcq, (int)destq, (int)destinst);
#endif

  if (running){
    while(running) {
#if defined(DEBUG) && defined(THREADS)
      counter++;
#endif
      running->inst_ptr = destinst;
      last = running;
      running = running->next_handle;
    }
    last->next_handle = *destq;
    if (last->next_handle)
      last->next_handle->previous_handle = last;
    *destq = *srcq;
    *srcq = NULL;
  }
#if defined(DEBUG) && defined(THREADS)
  if (debug_conc)
    fprintf(stderr,
            "*** %d(%d) after moving queue made %d steps\n",
            (int)Thread_Id, (int)GET_INC_COUNTER, counter);
#endif
}


/* Remove the linked chains which point to the calls to concurrent
   predicates which were suspended.  Start at topdynamic (topmost dynamic
   choicepoint) and go down the choicepoint stack until the next dynamic
   choicepoint to be considered is older than chpttoclear.  Then, return the
   value of that dynamic choicepoint in the variable topdynamic (it is the
   topmost dynamic choicepoint after the call!). */


void remove_link_chains(topdynamic, chpttoclear)
     CIAO_REGISTER node_t **topdynamic, *chpttoclear;
{
  CIAO_REGISTER node_t *movingtop = *topdynamic;
#if defined(DEBUG) && defined(THREADS)
  if (debug_conc)
    fprintf(stderr, "*** %d(%d) removing from %lx until %lx\n", 
            (int)Thread_Id, (int)GET_INC_COUNTER, 
            (unsigned long)*topdynamic,
            (unsigned long)chpttoclear);
#endif
  
  while (ChoiceYounger(movingtop, chpttoclear)){
#if defined(DEBUG) && defined(THREADS)
    if (debug_conc)
      fprintf(stderr, "*** %d(%d) removing handle at (dynamic) node %lx\n", 
              (int)Thread_Id, (int)GET_INC_COUNTER, 
              (unsigned long)movingtop);
#endif

    Cond_Begin(TagToRoot(movingtop->term[RootArg])->clause_insertion_cond);

#if defined(DEBUG)
    if (TagToInstHandle(movingtop->term[X2_CHN]) == NULL)
      fprintf(stderr, "*** %d(%d) remove_link_chains: X2 handle is NULL!!\n",
              (int)Thread_Id, (int)GET_INC_COUNTER);
    if (TagToInstHandle(movingtop->term[X5_CHN]) == NULL)
      fprintf(stderr, "*** %d(%d) remove_link_chains: X5 handle is NULL!!\n",              (int)Thread_Id, (int)GET_INC_COUNTER);
#endif
    remove_handle(TagToInstHandle(movingtop->term[X2_CHN]), 
                  TagToRoot(movingtop->term[RootArg]),
                  X2);
    remove_handle(TagToInstHandle(movingtop->term[X5_CHN]), 
                  TagToRoot(movingtop->term[RootArg]),
                  X5);

    Broadcast_Cond(TagToRoot(movingtop->term[RootArg])->clause_insertion_cond);

    movingtop=(node_t *)TermToPointerOrNull(movingtop->term[PrevDynChpt]);
  }
#if defined(DEBUG) && defined(THREADS)
  if (debug_conc)
    fprintf(stderr, "*** %d(%d) remove_link_chains: done at %lx\n", 
            (int)Thread_Id, (int)GET_INC_COUNTER, 
            (unsigned long)movingtop);
#endif
  *topdynamic = movingtop;
}
