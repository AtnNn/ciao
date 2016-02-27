/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

#include "threads.h"
#include "datadefs.h"
#include "support.h"
#include "predtyp.h"
#include "task_areas.h"
#include "profile_defs.h"

#include <assert.h>

/* declarations for global functions accessed here */

#include "builtin_defs.h"
#include "wam_defs.h"
#include "inout_defs.h"

#if defined(DBG) || defined(DEBUG)
#include "streams_defs.h"
#endif

/* local declarations */

#if defined(DBG) || defined(DEBUG)
static tagged_t safe_deref(tagged_t t);
#endif

bool_t set_predtrace(Arg)
     Argdecl;
{
  tagged_t x;
  DEREF(x,X(0));
  if (!TagIsSmall(x))
    return FALSE;
  predtrace = (bool_t)GetSmall(x);
#if defined(PROFILE)
  if (profile||predtrace) stop_on_pred_calls = TRUE;
#else
  if (predtrace) stop_on_pred_calls = TRUE;
#endif
  return TRUE;
}


/* run_determ_c(goal) runs the goal and returns TRUE if goal is
   defined as a c predicate, even if that predicate fails.  Otherwise
   it returns false.
   */

extern bcp_t bootcode;                                          /* Shared */
#if defined(INTERNAL_CALLING)
extern bcp_t internal_calling;                                          /* Shared */
#endif
extern bcp_t startgoalcode;                                          /* Shared */
extern bcp_t startgoalcode_cont;                                     /* Shared */



/*
myDEREF(Xderef,X) 
     tagged_t Xderef, X;
{ 
  CIAO_REGISTER tagged_t m_i, m_j; 
  tagged_t aux1, aux2, aux3;
  tagged_t *aux4;
  m_i = X; 

  if (IsVar(m_i)) 
    do {
      aux1 = (tagged_t)(m_i) & POINTERMASK;
      aux2 = aux1 + MallocBase;
      aux4 = (tagged_t *)aux2;
      aux3 = *aux4;
      m_i = aux3;
      if (m_i == m_j)
	break;
    }
    while (IsVar(m_i=m_j)); 

  Xderef = m_i; 
}
*/

bool_t run_determ_c(Arg,goal)
     Argdecl;
     tagged_t goal;
{
  definition_t *func;
  CIAO_REGISTER int i;
  CIAO_REGISTER tagged_t *s;

/*
  prolog_display(Arg);
  printf("\n");
  */
  

  DEREF(goal,goal);
  /*myDEREF(goal,goal);*//* Was for debugging */
  /*func = find_definition(&prolog_predicates,goal,&w->structure,FALSE);*/
  func = find_definition(predicates_location,goal,&w->structure,FALSE);

  if (func==NULL) return FALSE;

  if (func->enter_instr == ENTER_C){
  
    for (i=func->arity, s=w->structure; --i>=0;)
      RefHeap(w->term[i],HeapOffset(s,i));
    
#if 0                                                        /* was GAUGE */
    return (*func->code.cinfo->procedure)(Arg);
#else
    return (*func->code.cinfo)(Arg);
#endif
  } else
    if (func->enter_instr == BUILTIN_CALL){
      w->next_insn = bootcode;          /* Should have been initialized */
      wam(w, NULL);
      return TRUE;
    }
  return FALSE;
}


/* CALLQ|call/1|goal=X(0)|exit_toplevel */

#if defined(DBG) || defined(DEBUG)
static tagged_t safe_deref(t)
     tagged_t t;
{
   CIAO_REGISTER tagged_t aux;
   
   DerefSwitch(t,aux,;);
 
   return (t & ~3);
}


void wr_tagged(Arg,t)
     Argdecl;
     tagged_t t;
{
  wr_tagged_rec(Arg,t);
  putchar('\n');
}


void wr_tagged_rec(Arg,t)
     Argdecl;
     tagged_t t;
{
  CIAO_REGISTER tagged_t temp;
  int arity,i;

  t = safe_deref(t);
  switch(TagOf(t)) {
  case LST:
    putchar('[');
    RefCar(temp,t);
    wr_tagged_rec(Arg,temp);
    RefCdr(temp,t);
    t = safe_deref(temp);
    while(TagIsLST(t))	{
      putchar(',');
      RefCar(temp,t);
      wr_tagged_rec(Arg,temp);
      RefCdr(temp,t);
      t = safe_deref(temp);
    }
    if(t!=atom_nil) {
      putchar('|');
      wr_tagged_rec(Arg,t);
    }
    putchar(']');
    break;
  case STR:
    if (STRIsLarge(t))
      goto number;
    wr_tagged_rec(Arg,TagToHeadfunctor(t));
    putchar('(');
    arity = Arity(TagToHeadfunctor(t));
    for(i=1; i<=arity; i++){
      if(i>1) putchar(',');
      RefArg(temp,t,i);
      wr_tagged_rec(Arg,temp);
    }
    putchar(')');
    break;
  case UBV:
  case SVA:
  case HVA:
  case CVA:
    print_variable(Arg,stream_user_output,t);
    break;
  case ATM:
    print_atom(Arg,stream_user_output,t);
    break;
  case NUM:
  number:
  print_number(Arg, stream_user_output,t);
  break;
  }
}
#endif

static ENG_FLT fzero = 0.0;    /* Shared, no locked */
static unsigned long *zeros = (unsigned long *)(&fzero);        /* Shared */

void checkasserts()
{
  assert((sizeof(ENG_INT) == 4));
  assert((sizeof(tagged_t *) == 4));
  assert((sizeof(ENG_FLT) == 8));
  assert((zeros[0]==0 && zeros[1]==0));
}


void wr_functor(s,func)
     char *s;
     definition_t *func;
{
  printf("%s: ",s);
  wr_functor_1(func);
  putchar('\n');
}

/* unused */
/*
static definition_t *which_parent(func)
     definition_t *func;
{
  CIAO_REGISTER definition_t *func1;

  do
    func1 = func,
    func = (definition_t *)TagToPointer(func1->printname);
  while (!(func1->printname & 2));
  return func;
}
*/

/* unused */
/*
static which_child(func)
     definition_t *func;
{
   CIAO_REGISTER int i; 
  definition_t *f1;  

  for (i=1, f1 = which_parent(func)->code.incoreinfo->subdefs;
       f1 != func;
       i++, f1 = (definition_t *)TagToPointer(f1->printname))
    ;

  return i;

  printf("Out of order!!\n");
}
*/

void wr_functor_1(func)
     definition_t *func;
{
  if (!(func->printname & 1))
    printf("%s/%d", GetString(func->printname), func->arity);
  else
    printf("(?)");
/*
    {
      putchar('(');
      wr_functor_1(which_parent(func));
      printf("-%d)/%d", which_child(func), func->arity);
    }
*/
} 

void display_term(Argdecl, tagged_t term, stream_node_t *stream, bool_t quoted);

void wr_call(Arg,s,func)
     Argdecl;
     char *s;
     definition_t *func;
{
  short i;

  printf("%s: ",s);

  if (!(func->printname & 1))
    {
      printf("%s", GetString(func->printname));
      if (func->arity > 0) {
        putchar('(');
        DEREF(X(0),X(0));
        display_term(Arg,X(0),Output_Stream_Ptr, TRUE);
        for (i = 1; i < func->arity; i++) printf(",_");
        putchar(')');
      }
    }
  else
    printf("(?)");

  putchar('\n');
}

#if defined(DBG) || defined(DEBUG)

void wr_functor_spec(Arg,t)
     Argdecl;
     tagged_t t;
{
  wr_tagged(Arg,t);
  printf("/%ld\n",Arity(t));
}

void wr_x(Arg,i)
     Argdecl;
     int i;
{
  wr_tagged(Arg,X(i));
}

void wr_y(Arg,i)
     Argdecl;
     int i;
{
  CIAO_REGISTER frame_t *E = w->frame;
  
  wr_tagged(Arg,Y(i));
}

#endif
