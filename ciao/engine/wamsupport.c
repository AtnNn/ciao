/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#include "datadefs.h"
#include "support.h"
#include "instrdefs.h"

/* declarations for global functions accessed here */

#include "wamsupport_defs.h"
#include "alloc_defs.h"

/* local declarations */

static try_node_t *get_null_alt(int arity);


bcp_t startgoalcode;		       /* WAM code to start a goal -- Shared */
bcp_t bootcode;		       /* WAM bootstrap to run bootgoal -- Shared */
#if defined(INTERNAL_CALLING)
bcp_t internal_calling;		       /* WAM bootstrap to run bootgoal -- Shared */
#endif
bcp_t startgoalcode_cont;    /* Specify cont. on success and failure -- Shared */
bcp_t contcode;/* continuations of FrameSize N after exceptions -- Shared */
bcp_t failcode;	      /* continuation of FrameSize 0 that fails -- Shared */
bcp_t exitcode;	  /* continuation of FrameSize 0 that exits wam -- Shared */

static insn_t insn_fail[1] = {FAIL};                              /* Shared */
try_node_t *null_alt = NULL;/* linked list of null alternatives - Shared*/

try_node_t *termcode;/* "clause" of arity 1 that exits wam -- Shared */
try_node_t *fail_alt;         /* null alternative, arity=0 -- Shared */

/* Find a null alt. of 'arity', or push a new one. */
static try_node_t *get_null_alt(arity)
     int arity;
{
  CIAO_REGISTER try_node_t *a;
  ENG_INT current_mem = total_mem_count;

  for (a = null_alt; a; a = (a+1)->next)
    if (a->node_offset == ArityToOffset(arity)) return a;

  a = (try_node_t *)checkalloc(sizeof(try_node_t)
				    + sizeof(try_node_t *)
#if GAUGE
				    + 2*sizeof(ENG_INT)
#endif
				    );

  INC_MEM_PROG((total_mem_count - current_mem));

  a->node_offset = ArityToOffset(arity);
  a->number = 0;
  a->emul_p = insn_fail;
  a->emul_p2 = insn_fail;
  (a+1)->next = null_alt;	/* tail of list */
#if GAUGE
  a->entry_counter = (ENG_INT *)(a+1)+1;
  a->entry_counter[0] = 0;
  a->entry_counter[1] = 0;
#endif
  a->next = NULL;		/* no more alternatives */
  null_alt = a;
  return a;
}


/* return no. of bytes to skip if first alt. in read mode */
int p2_offset(insn)
     int insn;
{
  if
    ((insn==GET_NIL_X0) ||
     (insn==GET_LIST_X0))
    return 1;
  else if
    ((insn==GET_CONSTANT_X0) ||
     (insn==GET_STRUCTURE_X0))
      return 1+BPT;
  else if
    ((insn==GET_CONSTANT_X0Q) ||
     (insn==GET_STRUCTURE_X0Q))
      return 2+BPT;
  else return 0;
}

try_node_t *def_retry_c(proc,arity)
     bool_t (*proc)();
     int arity;
{
  try_node_t *item;
  bcp_t bp;

  item = (try_node_t *) checkalloc(sizeof(try_node_t)
                                        +sizeof(emul_info_t)
                                        +(2+BPTP)*sizeof(insn_t)
#if GAUGE
                                        +2*sizeof(ENG_INT)
#endif
                                        );
  item->node_offset = ArityToOffset(arity);
  item->number = 0;
  bp = item->emul_p = (bcp_t )(((char *)item)+sizeof(try_node_t));
  item->emul_p2 = bp;
  item->next = item;
  *bp++ = RETRY_CQ;
  *bp++ = 0;			/* alignment */
  *(short **)bp = (short *)proc;
  bp += BPTP;
#if GAUGE
  item->entry_counter = (ENG_INT *)bp;
  item->entry_counter[0] = 0;
  item->entry_counter[1] = 0;
#endif
  return item;
}

/*
   |CALLQ|0|address_call/1|...padding...|Initial Frame Size|EXIT_TOPLEVEL|
   ^                                                       ^
   bootcode                                                termcode?
*/

void init_bootcode(b)
     bcp_t b;
{
  bootcode = b;
  *b++ = CALLQ;
  *b++ = 0;			                         /* for alignment */
  *(definition_t **)b = address_call;
  b += BPTP;                                                   /* padding */
  *b++ = EToY0*sizeof(tagged_t);	                     /* initial FrameSize */
  *b++ = EXIT_TOPLEVEL;

  exitcode = b-2;
  termcode = def_retry_c(NULL,1);                 /* size of initial cpt. */
  b = termcode->emul_p;
  *b++ = EXIT_TOPLEVEL;

  address_nd_current_instance = def_retry_c(NULL,DynamicPreserved);
  b = address_nd_current_instance->emul_p;
  *b++ = RETRY_INSTANCE;
}

#if defined(INTERNAL_CALLING)
void init_internal_calling(b)
     bcp_t b;
{
  internal_calling = b;
  *b++ = CALLQ;
  *b++ = 0;			                         /* for alignment */
  *(definition_t **)b = address_internal_call;
  b += BPTP;                                                   /* padding */
  *b++ = EToY0*sizeof(tagged_t);	                     /* initial FrameSize */
  *b++ = EXIT_TOPLEVEL;
}
#endif


/*
|CALLQ|0|address_call/1|...padding...|Init. Frame Size|EXIT_TOPLEVEL|
   ^                                                            ^
   bootcode                                                     termcode?
*/


void init_startgoalcode(b)
     bcp_t b;
{
  startgoalcode = b;
  *b++ = CALLQ;
  *b++ = 0;			                         /* for alignment */
  *(definition_t **)b = address_call;
  b += BPTP;                                                   /* padding */
  *b++ = EToY0*sizeof(tagged_t);	                     /* initial FrameSize */
  *b++ = EXIT_TOPLEVEL;

  /*
    exitcode = b-2;
    termcode = def_retry_c(NULL,1);
    b = termcode->emul_p;
    *b++ = EXIT_TOPLEVEL;
    
    address_nd_current_instance = def_retry_c(NULL,DynamicPreserved);
    b = address_nd_current_instance->emul_p;
    *b++ = RETRY_INSTANCE;
  */
}

/*
|CALLQ|0|address_call_with_cont/2|...padding...|Init. Frame Size|EXIT_TOPLEVEL|
   ^                                                            ^
   bootcode                                                     termcode?
*/

void init_startgoalcode_cont(b)
     bcp_t b;
{
  startgoalcode_cont = b;
  *b++ = CALLQ;
  *b++ = 0;			                         /* for alignment */
  *(definition_t **)b = address_call_with_cont;
  b += BPTP;                                                   /* padding */
  *b++ = EToY0*sizeof(tagged_t);	                     /* initial FrameSize */
  *b++ = EXIT_TOPLEVEL;
}


/* '&contcode[(1+LOffset)*(i+1)]'
    is a good continuation when interrupting 'foo/i'. */
void init_contcode(b,f)
     bcp_t b;
     bcp_t f;
{
  int i;

  fail_alt = get_null_alt(0);

  failcode = f+LOffset;
  *f++ = EToY0*sizeof(tagged_t);	/* FrameSize */
  *f++ = EXECUTE;
  *(definition_t **)f = address_fail;

  contcode = b+LOffset;
  for (i=0; i<ARITYLIMIT; i++)
    {
      *b++ = (EToY0+i)*sizeof(tagged_t); /* FrameSize */
      *b++ = KONTINUE;
    }
}

/*
  Note: evaluate return v in case of error;
*/
tagged_t evaluate(Arg, v)
     Argdecl;                        /* To have a Wam ready for (*Proc)() */
     CIAO_REGISTER tagged_t v;
{
  CIAO_REGISTER tagged_t t, u;
  CIAO_REGISTER TInfo Proc;

 restart:
  switch (TagOf(v))
    {
    case NUM:
      return v;

    case LST:
      DerefCdr(t,v);
      DerefCar(v,v);
      if (t==atom_nil)
	goto restart;
      else
	return v;

    case STR:
      if (STRIsLarge(v))
	return v;
      t = TagToHeadfunctor(v);
      Proc = incore_gethash(switch_on_function,t)->value.tinfo;
      if (Proc!=NULL)
	switch (Arity(t))
	  {
	  case 1:
	    RefArg(t,v,1);
	    return (*Proc)(Arg,t,NULL);
	  case 2:
	    RefArg(t,v,1);
	    RefArg(u,v,2);
	    return (*Proc)(Arg,t,u,NULL);
	  }

    default:
      return v;
    }
}
