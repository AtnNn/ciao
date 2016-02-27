/* Copyright (C) 1996,1997,1998, UPM-CLIP */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "datadefs.h"
#include "support.h"
#include "instrdefs.h"
#include "threads.h"
#include "task_areas.h"                 /* For register bank reallocation */

/* declarations for global functions accessed here */

#include "term_support_defs.h"
#include "main_defs.h"
#include "alloc_defs.h"

/* local declarations */

static BOOL c_term(Argdecl,
                   register TAGGED t,
                   int Xreg,
                   int FreeReg,
                   int x_variables,
                   TAGGED **trail_origo,
                   INSN **current_insn);
static BOOL prolog_constant_codes(Argdecl,
                                  BOOL atomp,
                                  BOOL numberp);
static INSN *emit_unify_void(Argdecl,
                             register INSN *P);
static void c_term_size(Argdecl,
                        register TAGGED t,
                        int temps,
                        int *cells,
                        int *maxtemps,
                        int *insns,
                        TAGGED **trail_origo);
static void c_term_trail_push(Argdecl,
                              TAGGED t,
                              TAGGED **trail_origo);
static void copy_it(Argdecl,
                    register TAGGED *loc);


#define TopOfOldHeap TagToHVA(w->global_uncond)
#define Xop(X)	(((X)+WToX0)*sizeof(TAGGED))
#define Xinv(X)	(((X)/sizeof(TAGGED))-WToX0)
#define ODDOP(Insn) \
  if (((unsigned int)P)&3) \
    EMIT(Insn-0); \
  else \
    EMIT(Insn-1), *P++ = 0;
#define EVENOP(Insn) \
  if (!(((unsigned int)P)&3)) \
    EMIT(Insn-0); \
  else \
    EMIT(Insn-1), *P++ = 0;
#define EMIT(I) Last_Insn=P, *P++ = (I)


#define GCTEST(Pad) \
{ \
  if (HeapDifference(w->global_top,Heap_End) < (Pad)) \
    heap_overflow(Arg,Pad); \
  if (ChoiceDifference(w->node,w->trail_top) < (Pad)) \
    choice_overflow(Arg,Pad); \
}

static void copy_it PROTO((struct worker *w, TAGGED *loc));

void choice_overflow PROTO((Argdecl, int pad));
void number_to_string PROTO((Argdecl,TAGGED term, int base));
void checkdealloc PROTO((TAGGED *ptr, int decr));
int bn_from_string PROTO((char *x, Bignum *z, Bignum *zmax));
void explicit_heap_overflow PROTO((Argdecl, int pad, int arity));
void bn_to_string PROTO((Argdecl,Bignum *x, int base));

void push_choicept PROTO((Argdecl, struct try_node *alt));
void push_frame PROTO((Argdecl, int arity));
void pop_frame PROTO((Argdecl));
void pop_choicept PROTO((Argdecl));

int heap_overflow PROTO((Argdecl, int pad));

/* I believe the following must be shared and locked, because they are
   related to code compilation */


/*static INSN*/
 /* *current_insn, */        /* Inside compile_term_aux and passed around */
 /* *last_insn;    */                          /* Inside compile_term_aux */


/*static int*/
 /* x_variables, */       /* Now inside compile_term_aux and pased around */
 /* cells,       */                         /* Inited in compile_term_aux */
 /* insns,       */                         /* Inited in compile_term_aux */
 /* maxtemps;    */                         /* Inited in compile_term_aux */

/*static TAGGED*/
 /* *trail_origo;*/                         /* Inited in compile_term_aux */


static void c_term_trail_push(Argdecl, TAGGED t, TAGGED **trail_origo);
static void c_term_size(Argdecl, TAGGED t, int temps, int *cells,
                        int *maxtemps, int *insns, TAGGED **trail_origo);
static INSN *emit_unify_void(Argdecl, INSN *P);
static BOOL c_term(Argdecl, TAGGED t, int Xreg, int FreeReg,
                   int x_variables, TAGGED **trail_origo,
                   INSN **current_insn);
BOOL compile_term(Argdecl, struct worker **new_worker);
struct instance *compile_term_aux(Argdecl, TAGGED head, TAGGED body,
                                  struct worker **new_worker);



static void c_term_trail_push(Arg,t, trail_origo)
     Argdecl;
     TAGGED t;
     TAGGED **trail_origo;
{
  if (!ChoiceDifference(w->node,w->trail_top)) {
    REGISTER TAGGED *tr = w->trail_top;
    int reloc;

    choice_overflow(Arg,-CHOICEPAD);
    reloc = (char *)w->trail_top - (char *)tr;
    *trail_origo = (TAGGED *)((char *)*trail_origo + reloc);
    tr = w->trail_top;
    while (tr[-1] & QMask)
      TrailPop(tr) += reloc;
    while (TrailYounger(tr,*trail_origo+DynamicPreserved))
      CTagToPointer(TrailPop(tr)) += reloc;
  }
  TrailPush(w->trail_top,t);
}



static void c_term_size(Arg, t, temps, cells, maxtemps, insns, trail_origo)
     Argdecl;
     REGISTER TAGGED t;
     int temps;
     int *cells, *maxtemps, *insns;
     TAGGED **trail_origo;
{
  REGISTER TAGGED t1;
  REGISTER int i, arity;

 start:
  DerefHeapSwitch(t,t1,{goto var_size;});
  if (!(t & TagBitComplex))
    {
      if (t&QMask && t&TagBitFunctor)
	{
	  if (!(t&3))		/* unify_variable */
	    *insns += 1,
	    CTagToPointer(CTagToPointer(t))++;
	  else			/* unify_value */
	    *insns += 1;
	}
      else if (t!=atom_nil)
	*insns += 1+BPT;	/* unify_ + pad + constant */
      return;
    }
  else if (!(t & TagBitFunctor))
    {
      *insns += 5;		/* unify_ + xi + get_list + xj + 2*u */
      *cells += 2;
      if (*maxtemps < temps)
	*maxtemps = temps;
      RefCar(t1,t);
      c_term_size(Arg, t1, temps+1, cells, maxtemps, insns, trail_origo);
      RefCdr(t,t);
      goto start;
    }
  else
    {
      if (STRIsLarge(t))
	{
	  arity = LargeInsns(TagToHeadfunctor(t));
	  *insns += 5+arity;
	  /* unify_ + xi + get_large + xj + pad + functor + largeNum */
	  *cells += 1+(arity>>1);
	  return;
	}
      arity = Arity(TagToHeadfunctor(t));
      *insns += 4+BPT+arity;	/* unify_ + xi + get_structure
				   + pad + functor + xj + arity*u */
      *cells += 1+arity;
      if (*maxtemps < temps)
	*maxtemps = temps;
      for (i=1; i<arity; i++)
	{
	  RefArg(t1,t,i);
	  c_term_size(Arg, t1, temps+arity-i, cells, maxtemps, insns, trail_origo);
	}
      RefArg(t,t,arity);
      goto start;
    }
 var_size:
  if (t & TagBitCVA)
    {
      CTagToPointer(t) = Tag(ATM,w->trail_top)|QMask|2;
      c_term_trail_push(Arg,t,trail_origo);
      *insns += 5;	/* unify_variable + xi
			   + get_constraint + xj + 2*u */
      if (*maxtemps < temps)
	*maxtemps = temps;
      t = Tag(LST,TagToGoal(t));
      goto start;
    }
  else
    {
      CTagToPointer(t) = Tag(ATM,w->trail_top)|QMask;
      c_term_trail_push(Arg,t,trail_origo);
      *insns += 1;		/* unify_variable + xi */
    }
  return;
}

static INSN *emit_unify_void(Arg, P)
     Argdecl;
     REGISTER INSN *P;
{
  switch (*Last_Insn)
    {
    case UNIFY_VOID_1:
    case UNIFY_VOID_2:
    case UNIFY_VOID_3:
      (*Last_Insn)++;
      break;
    case UNIFY_VOID_4:
      (*Last_Insn) = UNIFY_VOID;
      *P++ = 5;
      break;
    case UNIFY_VOID:
      (*(P-1))++;
      break;
    default:
      EMIT(UNIFY_VOID_1);
    }
  return P;
}

static BOOL c_term(Arg,t,Xreg,FreeReg, x_variables, trail_origo,
                   current_insn)
     Argdecl;
     REGISTER TAGGED t;
     int Xreg, FreeReg;
     int x_variables;
     TAGGED **trail_origo;
     INSN **current_insn;
{
  INSN *psave;
  TAGGED *ssave;
  int i, ar, decr, Treg;
  REGISTER INSN *P = *current_insn;
  REGISTER TAGGED *s;
  REGISTER TAGGED t1;

  /* 1:  Emit GET instruction for term's principal functor. */

  switch (TagOf(t))		/* t is already dereferenced */
    {
    case LST:
      ar = 2;
      s = TagToLST(t);
      EMIT(GET_LIST);
      *P++ = Xop(Xreg);
      break;
    case STR:
      if (STRIsLarge(t))
	{
	  EVENOP(GET_LARGE);
	  *P++ = Xop(Xreg);
	  P += compile_large(t, P);
	  *current_insn = P;
	  return TRUE;
	}
      ar = Arity(TagToHeadfunctor(t));
      s = TagToArg(t,1);
      EVENOP(GET_STRUCTURE);
      *P++ = Xop(Xreg);
      *(TAGGED *)P = TagToHeadfunctor(t);
      P += BPT;
      break;
    case ATM:
      if (t & QMask)
	goto term_is_var;
      else if (t==atom_nil)
	{
	  EMIT(GET_NIL);
	  *P++ = Xop(Xreg);
	  *current_insn = P;
	  return TRUE;
	}
    case NUM:
      EVENOP(GET_CONSTANT);
      *P++ = Xop(Xreg);
      *(TAGGED *)P = t;
      P += BPT;
      *current_insn = P;
      return TRUE;

    term_is_var:
      {
	if ((t&3)!=3)		/* get_variable */
	  {
	    EMIT(GET_X_VARIABLE);
	    *P++ = Xop(Xreg);
	    *P++ = Xop(TagToPointer(t) - *trail_origo);
	    if (t&2)		/* enqueue constraint */
	      c_term_trail_push(Arg,t,trail_origo);
	    CTagToPointer(CTagToPointer(t)) |= 3;
	  }
	else			/* get_value */
	  {
	    EMIT(GET_X_VALUE);
	    *P++ = Xop(Xreg);
	    *P++ = Xop(TagToPointer(t) - *trail_origo);
	  }
	*current_insn = P;
	return TRUE;
      }
    }

  /* 2. Emit tail-recursive UNIFY sequence for all subargs. */

  psave = P;
  ssave = s;
  Treg = reg_bank_size;

  for (i=1; i<=ar; i++)
    {
      RefHeapNext(t,s);
      DerefHeapSwitch(t,t1,goto arg_is_void;);
      switch (TagOf(t))
	{
	case LST:
	  if ((i==ar) && (Treg==reg_bank_size))
	    {
	      EMIT(UNIFY_LIST);
	      s = TagToLST(t);
	      i=0, ar=2;
	    }
	  else
	    {
	      EMIT(UNIFY_X_VARIABLE);
	      *P++ = Xop(Treg++);
	    }
	  break;
	case STR:
	  if (STRIsLarge(t))
	    {
	      if ((i==ar) && (Treg==reg_bank_size))
		{
		  ODDOP(UNIFY_LARGE);
		  P += compile_large(t, P);
		}
	      else
		{
		  EMIT(UNIFY_X_VARIABLE);
		  *P++ = Xop(Treg++);
		}
	    }
	  else if ((i==ar) && (Treg==reg_bank_size))
	    {
	      ODDOP(UNIFY_STRUCTURE);
	      *(TAGGED *)P = TagToHeadfunctor(t);
	      P += BPT;		
	      s = TagToArg(t,1);
	      i=0, ar=Arity(TagToHeadfunctor(t));
	    }
	  else
	    {
	      EMIT(UNIFY_X_VARIABLE);
	      *P++ = Xop(Treg++);
	    }
	  break;
	case ATM:
	  if (t & QMask)
	    goto arg_is_var;
	  else if (t==atom_nil)
	    {
	      EMIT(UNIFY_NIL);
	      break;
	    }
	case NUM:
	  ODDOP(UNIFY_CONSTANT);
	  *(TAGGED *)P = t;
	  P += BPT;
	  break;

	arg_is_var:
	  {
	    if ((t&3)!=3)	/* unify_variable */
	      {
		EMIT(UNIFY_X_VARIABLE);
		*P++ = Xop(TagToPointer(t) - *trail_origo);
		if (t&2)	/* enqueue constraint */
		  c_term_trail_push(Arg,t,trail_origo);
		CTagToPointer(CTagToPointer(t)) |= 3;
	      }
	    else		/* unify_value */
	      {
		EMIT(UNIFY_X_VALUE);
		*P++ = Xop(TagToPointer(t) - *trail_origo);
	      }
	    break;
	  }

        arg_is_void:
	  P = emit_unify_void(Arg, P);
	  break;
	}
    }

  /* 3. Scan emitted code and recursively emit code for nested args. */

  *current_insn = P;
  if (FreeReg < x_variables)
    return FALSE;
  if (Treg==reg_bank_size)
    return TRUE;
  decr = Treg-1-FreeReg;
  s = ssave;
  P = psave;
  psave = *current_insn;

  while (P < psave)
  {
#if defined(APOLLO_CC_BUG)
    P++;
    switch (*(P-1))
#else
    switch (*P++)
#endif
      {
      case UNIFY_LIST:
	DerefHeap(t,s);
	s = TagToLST(t);
	break;
      case UNIFY_STRUCTUREQ:
	P++;
      case UNIFY_STRUCTURE:
	P += BPT;
	DerefHeap(t,s);
	s = TagToArg(t,1);
	break;
      case UNIFY_LARGEQ:
	P++;
      case UNIFY_LARGE:
	P += LargeInsns(*(TAGGED *)P);
	(void)HeapNext(s);
	break;
      case UNIFY_CONSTANTQ:
	P++;
      case UNIFY_CONSTANT:
	P += BPT;
      case UNIFY_NIL:
	(void)HeapNext(s);
	break;
      case UNIFY_X_VARIABLE:
	i = Xinv(*P);
	if (i>=reg_bank_size)
	  {
	    *P++ = Xop(i-decr);
	    DerefHeapNext(t,s);
	    if (!c_term(Arg,t,i-decr,i-decr,x_variables,
                        trail_origo,current_insn))
	      return FALSE;
	    break;
	  }
      case UNIFY_X_VALUE:
	P++;
	(void)HeapNext(s);
	break;
      case UNIFY_VOID:
	s += *P++;
	break;
      case UNIFY_VOID_4:
	(void)HeapNext(s);
      case UNIFY_VOID_3:
	(void)HeapNext(s);
      case UNIFY_VOID_2:
	(void)HeapNext(s);
      case UNIFY_VOID_1:
	(void)HeapNext(s);
	break;
      default:
	SERIOUS_FAULT("compile_term: internal error");
      }
  }
  return TRUE;
}


 /* ASSERT: X(0) is always a dereferenced list. */
 /* Also, returns in the second argument a pointer to a non-null worker
    pointer if the worker has changed, or to null if it has. */

BOOL compile_term(Arg, new_worker)
     Argdecl;
     struct worker **new_worker;
{
  TAGGED head, body;
  struct instance *object;


  RefCar(head,X(0));
  RefCdr(body,X(0));

  *new_worker = NULL;                             /* may be changed after */

  object = compile_term_aux(Arg, head, body, new_worker);
  Arg = *new_worker == NULL ? Arg : *new_worker;

  Unify_constant(PointerToTerm(object),X(1));
  return TRUE;
}

/* Note on memory consumption: compile_term_aux may increase the size of the
   stacks and the size of the X register bank, so the simple approach "see
   how much the total memory has increased" does not work.  Instead, we try
   to find out exactly where we allocate and deallocate memory for program
   storage.  c_term_size does not allocate program memory (instead, it may
   call choice_overflow, which expands stacks). */

struct instance *compile_term_aux(Arg, head, body, new_worker)
     Argdecl;
     TAGGED head, body;
     struct worker **new_worker;
{
  int lsize, truesize;
  REGISTER TAGGED t0, *pt1, *pt2;
  struct instance *object;
  
  int x_variables, cells, insns, maxtemps;
  TAGGED *trail_origo;
  INSN *current_insn /*, *last_insn */ ;

  insns=1-ANY, cells=CONTPAD, maxtemps=0;
  trail_origo = Arg->trail_top-DynamicPreserved;

  DerefHeapSwitch(head,t0,{goto car_done;});
  insns += 2;			/* get + arg */
  c_term_size(Arg, head, 0, &cells, &maxtemps, &insns, &trail_origo);
 car_done:
  DerefHeapSwitch(body,t0,{goto cdr_done;});
  insns += 2;			/* get + arg */
  c_term_size(Arg, body, 0, &cells, &maxtemps, &insns, &trail_origo);
 cdr_done:

				/* allow for heapmargin_call insn */
  if (cells>=CALLPAD)
    insns += 5;

				/* tidy out void vars */
  pt1 = pt2 = trail_origo+DynamicPreserved;
  while (pt1 < Arg->trail_top) {
    t0 = TrailNext(pt1);
    if (CTagToPointer(t0) & 3)
      CTagToPointer(t0) -= (char *)(pt1-1)-(char *)pt2,	TrailPush(pt2,t0);
    else
      CTagToPointer(t0) = t0;
  }
  Arg->trail_top = pt2;
  x_variables = pt2-trail_origo;

				/* ensure enough X registers */
  if (x_variables+maxtemps > reg_bank_size) {
      int d = SIZEOFWORKER(reg_bank_size);

      if (x_variables+maxtemps > (1<<14) - WToX0 - maxtemps)
	goto sizebomb;

      reg_bank_size=x_variables+maxtemps;

      *new_worker =                                /* For local use */
        (struct worker *)
        checkrealloc((TAGGED *)Arg, d, SIZEOFWORKER(reg_bank_size));
#if defined(DEBUG)
      fprintf(stderr, "Reallocing WRB from %lx to %lx\n",
              (long int)Arg, (long int)*new_worker);
#endif
      Arg = *new_worker;

      /*self = w = (struct worker *)checkrealloc((TAGGED *)w,d,SIZEOFWORKER(reg_bank_size));*/

    }


  lsize=sizeof(struct instance) + insns*sizeof(INSN);
  object=(struct instance *)checkalloc(lsize);
  INC_MEM_PROG(lsize);
  object->objsize = lsize;
  current_insn = object->emulcode;

  if (cells>=CALLPAD) {
    REGISTER INSN *P = current_insn;

    ODDOP(HEAPMARGIN_CALL);
    *(long *)P = cells;
    P += BPL;
    *P++ = DynamicPreserved;
    current_insn = P;
  }

  if (!IsVar(head) &&
      !c_term(Arg,head,0,reg_bank_size-1,x_variables,
              &trail_origo,&current_insn))
    goto sizebomb;
  if (!IsVar(body) &&
      !c_term(Arg,body,1,reg_bank_size-1,x_variables,
              &trail_origo,&current_insn))
    goto sizebomb;

  while (Arg->trail_top[-1] & QMask) {
    INSN *P = current_insn;

    if (!TrailYounger(Arg->trail_top,Trail_Start))
      break;
    t0 = TrailPop(Arg->trail_top);
    if (!c_term(Arg,
                Tag(LST,TagToGoal(CTagToPointer(t0))),
                TagToPointer(t0)-trail_origo,
                  reg_bank_size-1,
                x_variables,
                &trail_origo,
                &current_insn))
      goto sizebomb;
    *P = GET_CONSTRAINT;
  }

  *current_insn++ = DYNAMIC_NECK_PROCEED;
  truesize=sizeof(struct instance) +
                 (current_insn - object->emulcode - ANY)*sizeof(INSN);
  if (truesize>lsize) {
    checkdealloc((TAGGED *)object,lsize);
    DEC_MEM_PROG(lsize);
    SERIOUS_FAULT("bug: memory overrun in assert or record");
  }

  if (IsVar(head))
    ;				/* findall record---make it fast */
  else {
    object->objsize = truesize;
    object=(struct instance *)checkrealloc((TAGGED *)object, lsize, truesize);
    INC_MEM_PROG((truesize - lsize));
  }

  pt2 = Arg->trail_top;
  Arg->trail_top = trail_origo+DynamicPreserved;
  while (TrailYounger(pt2,Arg->trail_top))
    PlainUntrail(pt2,t0,;)

  if (TagIsSTR(head))  {
    DerefArg(t0,head,1);
    if (TagIsSTR(t0))
      object->key = TagToHeadfunctor(t0);
    else if (TagIsLST(t0))
      object->key = functor_list;
    else if (!IsVar(t0))
      object->key = t0;
    else
      object->key = ERRORTAG;
  } else
    object->key = ERRORTAG;

  return object;

 sizebomb:
  checkdealloc((TAGGED *)object,lsize);
  DEC_MEM_PROG(lsize);
  SERIOUS_FAULT("term too large in assert or record");
}


#if defined(OLD_DATABASE)
/* Support for current_key/2: given a '$current instance'/2 instance,
   the key of which is a large number, decode the key. */
TAGGED decode_instance_key(inst)
     struct instance *inst;
{
  REGISTER INSN *p = inst->emulcode;
  int xreg = -1;

  for (;;)
  {
#if defined(APOLLO_CC_BUG)
    p++;
    switch (*(p-1))
#else
    switch (*p++)
#endif
      {
      case HEAPMARGIN_CALLQ:
	p++;
      case HEAPMARGIN_CALL:
	p += BPT+1;
	break;
      case GET_CONSTANTQ:
      case GET_STRUCTUREQ:
	p++;
      case GET_CONSTANT:
      case GET_STRUCTURE:
	p += 1+BPT;
	break;
      case GET_LARGEQ:
	p++;
      case GET_LARGE:
	if (xreg == Xinv(*p))
	  return MakeLarge(p+1);
	p += 1+LargeInsns(*(TAGGED *)(p+1));
	break;
      case UNIFY_CONSTANTQ:
      case UNIFY_STRUCTUREQ:
	p++;
      case UNIFY_CONSTANT:
      case UNIFY_STRUCTURE:
	p += BPT;
	break;
      case UNIFY_LARGEQ:
	p++;
      case UNIFY_LARGE:
	p += LargeInsns(*(TAGGED *)p);
	break;
      case UNIFY_X_VARIABLE:
	if (xreg == -1)
	  xreg = Xinv(*p);
      case UNIFY_X_VALUE:
      case UNIFY_VOID:
      case GET_LIST:
      case GET_CONSTRAINT:
      case GET_NIL:
	p++;
	break;
      case GET_X_VARIABLE:
      case GET_X_VALUE:
	p += 2;
      default:
	break;
      }
  }
}
#endif



static int              /* Shared, no locked --- but it should be private! */
  radixlim1,
  radixlim2,
  radixlim3;

BOOL prolog_init_radix()
{
  int radix = GetSmall(current_radix);

  if (radix<10)
    radixlim1 = '0'+radix,
    radixlim2 = 'a',
    radixlim3 = 'A';
  else
    radixlim1 = '0'+10,
    radixlim2 = 'a'+radix-10,
    radixlim3 = 'A'+radix-10;
  return TRUE;
}


static BOOL prolog_constant_codes PROTO((struct worker *w,BOOL a,BOOL n));

BOOL prolog_name(Arg)
     Argdecl;
{
  return prolog_constant_codes(Arg,TRUE,TRUE);
}

BOOL prolog_atom_codes(Arg)
     Argdecl;
{
  return prolog_constant_codes(Arg,TRUE,FALSE);
}

BOOL prolog_number_codes(Arg)
     Argdecl;
{
  return prolog_constant_codes(Arg,FALSE,TRUE);
}


static BOOL prolog_constant_codes(Arg,atomp,numberp)
     Argdecl;
     BOOL atomp, numberp;
{
  REGISTER unsigned char *s;
  REGISTER int i;
  REGISTER TAGGED car, cdr;

  /*extern ENG_FLT atof PROTO((char *));*/

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  if (IsVar(X(0))) {
    cdr = X(1);
    s = (unsigned char *)Atom_Buffer;
    for (i=0; cdr!=atom_nil; i++) {
      if (IsVar(cdr))
        BUILTIN_ERROR(INSTANTIATION_ERROR,atom_nil,2)
        else if (!TagIsLST(cdr))
          BUILTIN_ERROR(TYPE_ERROR(CHARACTER_CODE_LIST),X(1),2)
            else if (i == Atom_Buffer_Length){
                 Atom_Buffer = (char *)checkrealloc((TAGGED *)Atom_Buffer,
                                                    i, Atom_Buffer_Length<<=1);
                      s = (unsigned char *)Atom_Buffer+i;
            }
      DerefCar(car,cdr);
      if (IsVar(car))
        BUILTIN_ERROR(INSTANTIATION_ERROR,atom_nil,2)
        if (!TagIsSmall(car) || (car<=TaggedZero) || (car>=MakeSmall(256)))
          BUILTIN_ERROR(TYPE_ERROR(CHARACTER_CODE_LIST),X(1),2)
	  *s++ = GetSmall(car);
      DerefCdr(cdr,cdr);
    }
    if (i == Atom_Buffer_Length) {
      Atom_Buffer = (char *)checkrealloc((TAGGED *)Atom_Buffer,
                                         i, Atom_Buffer_Length<<=1);
      s = (unsigned char *)Atom_Buffer+i;
    }
    *s++ = '\0';
    if (i>=MAXATOM) atomp = FALSE;  /* Unneded with dynamic atom sizes */

    /* INTEGER :== [minus]{digit} */
    /* FLOAT :== [minus]{digit}.{digit}[exp[sign]{digit}] */
    /* ATOM :== {char} */

    if (numberp) {
      BOOL sign = FALSE;
	
      s = (unsigned char *)Atom_Buffer;
      i = *s++;
      if (i=='-')
        sign = TRUE, i = *s++;
      while (((i>='0') && (i<radixlim1)) ||
             ((i>='a') && (i<radixlim2)) ||
             ((i>='A') && (i<radixlim3)))
        i = *s++;
      if ((char *)s-Atom_Buffer-sign>1) {
        if (i==0) {
          REGISTER TAGGED *h = w->global_top;
          TAGGED t;
          int req = bn_from_string(Atom_Buffer, h, Heap_End-CONTPAD);

          if (req) {
            explicit_heap_overflow(Arg,req+CONTPAD, 2);
            h = w->global_top;
            if (bn_from_string(Atom_Buffer, h, Heap_End-CONTPAD))
              SERIOUS_FAULT("miscalculated size of bignum");
          }
          req = LargeArity(h[0]);
          if (req==2 && IntIsSmall((int)h[1]))
            t = MakeSmall(h[1]);
          else {
            w->global_top += req+1;
            h[req] = h[0];
            t = Tag(STR,h);
          }
          return cunify(Arg, t, X(0));
        }
        if (i=='.'){
          i = *s++;
          if ((i>='0') && (i<='9')) {
            do
              i = *s++;
            while ((i>='0') && (i<='9'));
            if ((i=='e') || (i=='E')){
              i = *s++;
              if ((i=='+') || (i=='-'))
                i = *s++;
              if ((i>='0') && (i<='9')) {
                do
                  i = *s++;
                while ((i>='0') && (i<='9'));
              } else
                i = -1;
            }
          }
          else
            i = -1;
        }
        if (i==0)
          return cunify(Arg,MakeFloat(Arg,atof(Atom_Buffer)),X(0));
      }
    }
    return (atomp && cunify(Arg,init_atom_check(Atom_Buffer),X(0)));
  } else {
    if (numberp && IsNumber(X(0)))
      number_to_string(Arg,X(0), GetSmall(current_radix)),
      s = (unsigned char *)Atom_Buffer;
    else if (atomp && TagIsATM(X(0)))
      s = (unsigned char *)GetString(X(0));
    else
      if (numberp) {
        if (atomp)
          BUILTIN_ERROR(TYPE_ERROR(ATOMIC),X(0),1)
          else
            BUILTIN_ERROR(TYPE_ERROR(NUMBER),X(0),1)
          } else
            BUILTIN_ERROR(TYPE_ERROR(ATOM),X(0),1)

              s += (i = strlen(s));
    if (HeapDifference(w->global_top,Heap_End)<CONTPAD+(i<<1))
      explicit_heap_overflow(Arg,CONTPAD+(i<<1),2);

    cdr = atom_nil;
    while (i>0)	{
      i--;
      MakeLST(cdr,MakeSmall(*(--s)),cdr);
    }
    return cunify(Arg,cdr,X(1));
  }
}

#define EXPAND_ATOM_BUFFER(new_atom_length) \
{ \
  int i = Atom_Buffer_Length; \
  while (Atom_Buffer_Length < new_atom_length) \
    Atom_Buffer_Length<<=1; \
  Atom_Buffer = (char *)checkrealloc((TAGGED *)Atom_Buffer, \
                                     i, Atom_Buffer_Length); \
}

BOOL prolog_atom_length(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));

  if (!TagIsATM(X(0)))
    ERROR_IN_ARG(X(0),1,ATOM);

  if (!IsInteger(X(1)) && !IsVar(X(1)))
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(1),2)

#if defined(USE_ATOM_LEN)
  return cunify(Arg,MakeSmall(GetAtomLen(X(0))),X(1));
#else
  return cunify(Arg,MakeSmall(strlen(GetString(X(0)))),X(1));
#endif
}

/* sub_atom(Atom, Before, Lenght, Sub_atom) */
BOOL prolog_sub_atom(Arg)
     Argdecl;
{
  unsigned char *s, *s1;
  int l, b, atom_length;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  DEREF(X(3),X(3));

  if (!TagIsATM(X(0)))
    ERROR_IN_ARG(X(0),1,ATOM);
  if (!IsInteger(X(1)))
    ERROR_IN_ARG(X(1),2,INTEGER);
  if (!IsInteger(X(2)))
    ERROR_IN_ARG(X(2),3,INTEGER);

  s = (unsigned char *)GetString(X(0));
#if defined(USE_ATOM_LEN)
    l = GetAtomLen(X(0));
#else
  l = strlen(s);
#endif

  b = GetInteger(X(1));
  if (b < 0 || b > l)
    return FALSE;

  atom_length = GetInteger(X(2));
  if (atom_length < 0 || atom_length+b > l)
    return FALSE;

  s += b;

  if (Atom_Buffer_Length <= atom_length)
    EXPAND_ATOM_BUFFER(atom_length+1);

  s1 = (unsigned char *)Atom_Buffer;

  strncpy(s1, s, atom_length);

  *(s1+atom_length) = '\0';

  return cunify(Arg,init_atom_check(Atom_Buffer),X(3));

}

BOOL prolog_atom_concat(Arg)
     Argdecl;
{
  int new_atom_length;
  unsigned char *s, *s1, *s2;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));

  if (TagIsATM(X(0))) {
    s1 = (unsigned char *)GetString(X(0));

    if (TagIsATM(X(1))) {

      if (!TagIsATM(X(2)) && !IsVar(X(2)))
        BUILTIN_ERROR(TYPE_ERROR(ATOM),X(2),3)
/* atom_concat(+, +, ?) */
      s2 = (unsigned char *)GetString(X(1));

#if defined(USE_ATOM_LEN)
      new_atom_length = GetAtomLen(X(0)) + GetAtomLen(X(1)) + 1;
#else
      new_atom_length = strlen(s1) + strlen(s2) + 1;
#endif

#if defined(USE_DYNAMIC_ATOM_SIZE)
      if (Atom_Buffer_Length < new_atom_length)
        EXPAND_ATOM_BUFFER(new_atom_length);
#else
      if (new_atom_length > MAXATOM)
#if defined(DEBUG)
        SERIOUS_FAULT("atom length exceeded in prolog_atom_concat()")
#else
        return FALSE; 
#endif
      if (Atom_Buffer_Length < new_atom_length)
        EXPAND_ATOM_BUFFER(new_atom_length);
#endif

      /* Append the two strings in atom_buffer */
      s = (unsigned char *)Atom_Buffer;
      while (*s1)
        *s++ = *s1++;
      while (*s2)
        *s++ = *s2++;
      *s = '\0';
      return cunify(Arg,init_atom_check(Atom_Buffer),X(2));

    } else if (IsVar(X(1))) {
      if (!TagIsATM(X(2)))
        { ERROR_IN_ARG(X(2),3,ATOM); }
/* atom_concat(+, -, +) */
      s2 = (unsigned char *)GetString(X(2));

#if defined(USE_ATOM_LEN)
      if ((new_atom_length = GetAtomLen(X(2))+1) > Atom_Buffer_Length)
        EXPAND_ATOM_BUFFER(new_atom_length);
#else
      if ((new_atom_length = strlen(s2)+1) > Atom_Buffer_Length)
        EXPAND_ATOM_BUFFER(new_atom_length);
#endif

      for ( ; *s1 && *s2 ; s1++, s2++)
        if (*s1 != *s2) return FALSE;

      if (*s1) return FALSE;

      s = (unsigned char *)Atom_Buffer;

      strcpy(s, s2);

      return cunify(Arg,init_atom_check(Atom_Buffer),X(1));

    } else
      BUILTIN_ERROR(TYPE_ERROR(ATOM),X(1),2);
  } else if (IsVar(X(0))) {
    if (!TagIsATM(X(2)))
        { ERROR_IN_ARG(X(2),3,ATOM); }

    if (TagIsATM(X(1))) {
/* atom_concat(-, +, +) */

      s1 = (unsigned char *)GetString(X(1));
      s2 = (unsigned char *)GetString(X(2));

#if defined(USE_ATOM_LEN)
      if ((new_atom_length = (GetAtomLen(X(2)) - GetAtomLen(X(1)))) < 0)
        return FALSE;
#else
      if ((new_atom_length = strlen(s2)-strlen(s1)) < 0)
        return FALSE;
#endif

      if (new_atom_length+1 > Atom_Buffer_Length)
        EXPAND_ATOM_BUFFER(new_atom_length+1);

      s = s2+new_atom_length;

      if (strcmp(s1, s)) /* different */
        return FALSE;

      s = (unsigned char *)Atom_Buffer;

      strncpy(s, s2, new_atom_length);

      *(s+new_atom_length) = '\0';

      return cunify(Arg,init_atom_check(Atom_Buffer),X(0));
    } else if (IsVar(X(1))) {
/* atom_concat(-, -, +) */

      s2 = (unsigned char *)GetString(X(2));
#if defined(USE_ATOM_LEN)
      if ((new_atom_length = GetAtomLen(X(2))+1) > Atom_Buffer_Length)
        EXPAND_ATOM_BUFFER(new_atom_length);
#else
      if ((new_atom_length = strlen(s2)+1) > Atom_Buffer_Length)
        EXPAND_ATOM_BUFFER(new_atom_length);
#endif
      X(3) = TaggedZero;
      push_choicept(Arg,address_nd_atom_concat);
      return nd_atom_concat(Arg);

    } else
      BUILTIN_ERROR(TYPE_ERROR(ATOM),X(1),2);
  } else
    BUILTIN_ERROR(TYPE_ERROR(ATOM),X(0),1);
}

/* Precond: 2<=abs(base)<=36 */
void number_to_string(Arg, term, base)
     Argdecl;
     TAGGED term;
     int base;
{
  if (TagIsSmall(term))
    {
      ENG_INT l = GetSmall(term);
      char hibase = 'a'-10;
      BOOL sx = (l>=0);
      ENG_INT digit;
      REGISTER char *c0, *c, d;

      if (base<0)
	hibase = 'A'-10,
	base = -base;
      c = Atom_Buffer;
      if (!sx)
	*c++ = '-',
	l = -l;

      do
	{
	  digit = l % base;
	  l /= base;
	  *c++ = (digit<10 ? '0'+digit : hibase+digit);
	}
      while (l>0);

      *c++ = 0;
      for (c0=Atom_Buffer+1-sx, c-=2; c0<c; c0++, c--)
	d = *c0, *c0 = *c, *c = d;
    }
  else if (IsFloat(term))
    {
      ENG_FLT f;
      ENG_INT li;
      unsigned int *fp = (unsigned int *)(&f);
      int exp = 8;
      REGISTER char *c, *c1;
      char *cbuf = Atom_Buffer;

      fp[0] = CTagToArg(term,1); /* f = GetFloat(term); */
      fp[1] = CTagToArg(term,2);
#if defined(sun)
      if (fp[1-BIGENDIAN] & 0x80000000)
#else
      if (f < 0.0 ||
	  (f == 0.0 && (fp[0]|fp[1]) != 0) ||
	  (fp[0]&fp[1]) == 0xffffffff)
#endif
	*cbuf++ = '-',
	f = -f;

      if (f == 0.0)
	exp = 0;
      else if (f != f || f == f/2.0) /* catch NaN, Infinity */
	exp = 1000,
	f = 1.0e+8;
      else
	{
	  /* normalize */
	  while (f >= 1.0e+9)
	    exp++, f /= 10.0;
	  while (f < 1.0e+8 && f > 0.0)
	    --exp, f *= 10.0;
	  /* compensate for truncations */
	  f += ENG_FLT_ROUND;
	  if (f >= 1.0e+9)
	    exp++, f /= 10.0;
	}
      /* print 18 digits */
      for (c=cbuf; c-18<cbuf; c+=9)
	{
	  li = f, f -= li, f *= 1.0e+9;
	  for (c1=c+9; c1>c;)
	    *(--c1) = '0'+li % 10, li /= 10;
	}

      /* strip insignificant digits */
      while (c>cbuf+ENG_FLT_SIGNIF)
	--c;
      /* strip trailing zeros */
      while (c[-1] == '0' && c>cbuf)
	--c;
      /* select 'e' or 'f' format */
      if (exp < -4 || exp > c-cbuf+2)
	{
	  for (c1=c-1; c1>=cbuf; --c1)
	    c1[1] = c1[0];
	  c1[2] = '.';
	  c = (c-cbuf == 1 ? c+2 : c+1);
	  *c++ = 'e';
	  if (exp<0)
	    *c++ = '-',
	    exp = -exp;
	
	  for (li=10000, c1=c; li>1; li /= 10)
	    if (exp>=li)
	      *c++ = '0' + exp / li,
	      exp %= li;
	    else if (c != c1)
	      *c++ = '0';
	  *c++ = '0' + exp;
	
	  *c++ = 0;
	}
      else if (exp >= c-cbuf-1)
	{
	  while (exp > c-cbuf-1)
	    *c++ = '0';
	  *c++ = '.';
	  *c++ = '0';
	  *c++ = 0;
	}
      else if (exp>=0)
	{
	  for (c1=c-1; c1-exp>=cbuf; --c1)
	    c1[1] = c1[0];
	  c1[2] = '.';
	  c[1] = 0;
	}
      else
	{
	  for (c1=c-1; c1-exp+1>=cbuf; --c1)
	    c1[1-exp] = (c1>=cbuf ? c1[0] : '0');
	  cbuf[1] = '.';
	  c[1-exp] = 0;
	}
    }
  else
    bn_to_string(Arg,TagToSTR(term),base);
}


/* copy_term(?Old,?New):
 * Algorithm:
 * If Old is a plain variable, just return.
 * Otherwise allocate a frame containing Old and New.
 * The frame slot for Old will be progressively replaced by a copy of Old.
 * Thus all relevant parts of the old and new structures are reachable from
 * the frame slot, should GC occur.
 * While copying, all old variables encountered are bound to their copies.
 * This requires a choicepoint.
 * Finally untrail but trail any new CVA:s, deallocate frame & choicept,
 * and unify copy with New.
 */
BOOL prolog_copy_term(Arg)
     Argdecl;
{

  REGISTER TAGGED t1, t2, *pt1, *pt2;

  t1 = X(0);
  SwitchOnVar(t1,t2,
	      return TRUE;,
	      ;,
	      return TRUE;,
	      ;);

  X(0) = t1;
  push_choicept(Arg,fail_alt);	/* try, arity=0 */
  push_frame(Arg,2);		/* allocate, size=2 */

  copy_it(Arg,&w->frame->term[0]); /* do the copying */

  pt1 = pt2 = TagToPointer(w->node->trail_top); /* untrail */
  while (!OffTrailtop(pt2,w->trail_top)) {
    t1 = TrailNext(pt2);	/* old var */
    CTagToPointer(t1) = t1;
  }
  w->trail_top = pt1;

  pop_frame(Arg);
  pop_choicept(Arg);		/* trust */
  return cunify(Arg,X(0),X(1));
}


static void copy_it(Arg,loc)
     Argdecl;
     REGISTER TAGGED *loc;
{
  REGISTER TAGGED t1, t2, *pt1, *pt2;
  REGISTER int i;
  int term_so_far;		/* size of new heap before copying subterms */

 start:
  RefHeap(t1,loc);
  SwitchOnHeapVar(t1,t2,{goto copy_hva;},{goto copy_cva;},;);

  if (IsAtom(t1) || IsNumber(t1)) {                           /* NUM, ATM */
    *loc = t1;
    return;
  } else if (t1 & TagBitFunctor) {                                 /* STR */
    pt1 = TagToSTR(t1);
    pt2 = w->global_top;
    *loc = Tag(STR,pt2);
    t2 = HeapNext(pt1), HeapPush(pt2,t2);
    for (i=Arity(t2); i>0; --i) {
      RefHeapNext(t1,pt1);
      HeapPush(pt2,t1);
    }
    w->global_top = pt2;
    term_so_far = HeapDifference(TopOfOldHeap,pt2);
    GCTEST(CHOICEPAD);
    for (i=Arity(t2); i>1; --i)
      copy_it(Arg,HeapOffset(TopOfOldHeap,term_so_far-i));
  } else {				                           /* LST */
    pt1 = TagToLST(t1);
    pt2 = w->global_top;
    *loc = Tag(LST,pt2);
  copy_2_cells:
    RefHeapNext(t1,pt1);
    HeapPush(pt2,t1);
    RefHeapNext(t1,pt1);
    HeapPush(pt2,t1);
    w->global_top = pt2;
    term_so_far = HeapDifference(TopOfOldHeap,pt2);
    GCTEST(CHOICEPAD);
    copy_it(Arg,HeapOffset(w->global_top,-2));
  }
  GCTEST(CHOICEPAD);
  loc = HeapOffset(TopOfOldHeap,term_so_far-1);
  goto start;

 copy_hva:
  if (CondHVA(t1)){		                                   /* HVA */
    PreLoadHVA(*loc,loc);
    t2 = TagHVA(loc);
    BindHVA(t1,t2);
  } else *loc = t1;
  return;

 copy_cva:
  if (CondCVA(t1)){		                       /* new 3-field CVA */
    pt1 = TagToGoal(t1);
    pt2 = w->global_top;
    LoadCVA(t2,pt2);
    BindCVA(t1,t2);
    *loc = t2;
    goto copy_2_cells;
  } else *loc = t1;
  return;
}



/* Copy a term in a remote worker to the local worker.  Returns the local
   term pointer.  It has (nontermination) problems when copying structures
   with self references. */

TAGGED cross_copy_term(Arg, remote_term)
     Argdecl;
     TAGGED remote_term;
{
  X(0) = remote_term;
  LoadHVA(X(1), w->global_top);
#if defined(DEBUG)
  if (!prolog_copy_term(Arg))
    fprintf(stderr, "Could not copy term in cross_copy_term!!!!\n");
#else
  prolog_copy_term(Arg);
#endif
  return X(1);
}
