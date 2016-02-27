/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

#include <unistd.h>
#include <string.h>

#include "datadefs.h"
#include "support.h"
#include "predtyp.h"

/* declarations for global functions accessed here */

#include "bignum_defs.h"
#include "support_defs.h"
#include "alloc_defs.h"
#include "stacks_defs.h"
#include "bignum_defs.h"
#include "locks_defs.h"
#include "start_defs.h"
#include "initial_defs.h"
#include "profile_defs.h"
#include "task_areas.h"
#include "tasks_defs.h"

/* local declarations */

static bool_t cunify_args_aux(Argdecl, register int arity, register tagged_t *pt1, register tagged_t *pt2, tagged_t *x1, tagged_t *x2);
static bool_t cunify_aux PROTO((worker_t *w, tagged_t x1, tagged_t x2));
static bool_t cunify_aux(Argdecl, tagged_t x1, tagged_t x2);
static int file_is_tty(FILE *file);
static void numstack_overflow(Argdecl);
static definition_t **find_subdef_chain(definition_t *f, int clause_no);
static definition_t *parse_1_definition(tagged_t tagname, tagged_t tagarity);
#if defined(USE_ATOM_LEN)
static sw_on_key_node_t *atom_gethash(register sw_on_key_t *sw, 
                                           tagged_t key, 
                                           char *str,
                                           unsigned int str_len);
#else
static sw_on_key_node_t *atom_gethash(register sw_on_key_t *sw, 
                                           tagged_t key, 
                                           char *str);
#endif


/*-----------------------------------------------------------*/

ENG_INT goal_from_thread_id_hook_(THREAD_ID id)
{
  return TRUE;
}

ENG_INT (*eng_goal_from_thread_id)(THREAD_ID id)=goal_from_thread_id_hook_;

void failc(mesg)
     char *mesg;
{
  extern char source_path[];

  // Give an error.  We check if we were able to allocate memory at all for
  // the user_eror stream (since we are using the same routines to allocate
  // all memory, either tagged or not, we may have failed to allocate
  // memory for streams).  This should not be necessary once we separate
  // the memory management 

  if (!stream_user_error) {
    fprintf(stderr, "{ERROR (%s): %s}\n", source_path, mesg);
    fprintf(stderr, 
"{Ciao was probably compiled in a machine with a different memory model.}\n");
    fprintf(stderr, 
"{Please recompile Ciao in this machine and try again.}\n");
  } else {
    // Issue a simple message if a single thread is running.
    if (num_tasks_created() > 1) {
      THREAD_ID thrid = Thread_Id;   // Local Id

      ENG_INT goal_id = eng_goal_from_thread_id(thrid);
      if (goal_id != 0) {
        ENG_PRINTF5(stream_user_error,
                    "{ERROR (%s, goal %d, thread %x/%d): %s}\n",
                    source_path, (unsigned int)goal_id,
                    (unsigned int)thrid, (unsigned int)thrid, mesg);
      } else {
        ENG_PRINTF4(stream_user_error,
                    "{ERROR (%s, thread %x/%d): %s}\n",
                    source_path,
                    (unsigned int)thrid, (unsigned int)thrid, mesg);
      }
    } else {
        ENG_PRINTF1(stream_user_error, "{ERROR: %s}\n", mesg);
    }
  }
  if (!wam_initialized){
    printf("Wam not initialized, exiting!!!\n");
    at_exit(-1);
  }
}

/*-----------------------------------------------------------*/

/* segfault patch -- jf */
void trail_push_check(Argdecl, tagged_t x) {
  CIAO_REG_1(tagged_t *, tr);
  tr = w->trail_top;

  TrailPush(tr,x);
  w->trail_top = tr;
  if (ChoiceYounger(w->node,TrailOffset(tr,CHOICEPAD)))
    choice_overflow(Arg,CHOICEPAD);
}

/*------------------------------------------------------------*/

/* insert atom in global table */
/*  MCL: there is an implicit assumption that the table is not full */

#if defined(USE_ATOM_LEN)
static sw_on_key_node_t *atom_gethash(sw, key, str, str_len)
     CIAO_REGISTER sw_on_key_t *sw;
     tagged_t key;
     char *str;
     unsigned int str_len;
#else
static sw_on_key_node_t *atom_gethash(sw, key, str)
     CIAO_REGISTER sw_on_key_t *sw;
     tagged_t key;
     char *str;
#endif
{
  CIAO_REGISTER sw_on_key_node_t *hnode;
#if defined(ATOMGC)
  CIAO_REGISTER sw_on_key_node_t *first_erased = NULL;
#endif
  CIAO_REGISTER int i;
  CIAO_REGISTER tagged_t t0;

  for (i=0, t0=key & sw->mask;
       ;
       i+=sizeof(sw_on_key_node_t), t0=(t0+i) & sw->mask) {
    hnode = (sw_on_key_node_t *)&sw->tab.aschar[t0];
#if !defined(ATOMGC)
    if ((hnode->key==key 
#if defined(USE_ATOM_LEN)
         && hnode->value.atomp->atom_len == str_len
#endif
         && strcmp(hnode->value.atomp->name, str)==SAME) ||
        !hnode->key)
      return hnode;
#else
    if ((hnode->key == key) 
#if defined(USE_ATOM_LEN)
        && hnode->value.atomp->atom_len == str_len
#endif
        && (strcmp(hnode->value.atomp->name, str) == SAME))
      return hnode;
    else if (!hnode->key)
      return first_erased ? first_erased : hnode;
    else if (hnode->key == 1 && !first_erased)
      first_erased = hnode;
#endif
  }
}



tagged_t init_atom_check(str)
     char *str;
{
  CIAO_REGISTER sw_on_key_node_t *hnode;
  unsigned int hashcode = 0;
  int count, size;
  ENG_INT current_mem = total_mem_count;
  CIAO_REGISTER unsigned char *c = (unsigned char *)str;

#if defined(USE_ATOM_LEN)
  unsigned int atom_len = 0;
#endif
  
  while (*c) {
    hashcode = (hashcode<<1) + *c++;
#if defined(USE_ATOM_LEN)
    atom_len++;
#endif
  }

  hashcode = (hashcode<<3)+4;	/* low bits are masked away; ensure it is
				   not 0 --- it cannot be 1, either, which is
				   very important for atom GC */
/*
  while ((hnode=incore_gethash(ciao_atoms, (tagged_t)hashcode)) &&
	 hnode->key==(tagged_t)hashcode &&
	 strcmp(hnode->value.atomp->name, str)!=SAME)
    hashcode += 233509<<3;         233509 is prime, and so is
				   233509&0x1ffff, 233509&0x0ffff, ...,
				   233509&0x00007
*/

#if defined(USE_ATOM_LEN)
  hnode = atom_gethash(ciao_atoms, (tagged_t)hashcode, str, atom_len);
#else
  hnode = atom_gethash(ciao_atoms, (tagged_t)hashcode, str);
#endif

#if defined(ATOMGC)
  if (hnode->key && hnode->key != 1) /* if ATOMGC, '1' marks freed position */
#else
  if (hnode->key)
#endif
    return MakeAtom(hnode->value.atomp->index);

  if ((count=ciao_atoms->count) > (INDEXMASK>>2)) {
    SERIOUS_FAULT("the atom table is full");
  }

  /* Check for a full table, and expand if needed */

  if ((count+1)<<1 > (size=SwitchSize(ciao_atoms))) {
    sw_on_key_t *new_table = new_switch_on_key(size<<1, NULL);
    CIAO_REGISTER int i;
    CIAO_REGISTER sw_on_key_node_t *h1, *h2;

#if defined(ATOMGC) && defined(DEBUG)
    /*printf("Reallocing atom table (count = %d)\n", count);*/
#endif

    for (i=0; i<count; i++){
#if defined(ATOMGC)   /* Actually, if the table is full, no entry should be
                         null... */
       /* size *= 2; */
      if ((h1 = atmtab[i]) != NULL) { /* There may be holes when doing GC */
#if defined(USE_ATOM_LEN)
        atmtab[i] = h2 = atom_gethash(new_table, 
                                      h1->key, 
                                      str,
                                      h1->value.atomp->atom_len);
#else
        atmtab[i] = h2 = atom_gethash(new_table, h1->key, str);
#endif
        h2->key = h1->key;
        h2->value.atomp = h1->value.atomp;
      }
#else
      h1 = atmtab[i];
#if defined(USE_ATOM_LEN)
      atmtab[i] = h2 = atom_gethash(new_table, 
                                    h1->key, 
                                    str,
                                    h1->value.atomp->atom_len);
#else
      atmtab[i] = h2 = atom_gethash(new_table, h1->key, str);
#endif
      h2->key = h1->key;
      h2->value.atomp = h1->value.atomp;
#endif
    }

    atmtab = (sw_on_key_node_t **)checkrealloc((tagged_t *)atmtab,
                                                    count*sizeof(h1),
                                                    2*count*sizeof(h1));

#if defined(ATOMGC)      /* Clean up the upper part of the new atom table */
    for (i = count; i < 2*count; i++)
      atmtab[i] = NULL;
    new_table->next_index = count;
#endif

    checkdealloc((tagged_t *)ciao_atoms,
                 sizeof(sw_on_key_t)+
                 (size-ANY)*sizeof(sw_on_key_node_t));
    new_table->count = count;
#if defined(USE_ATOM_LEN)
    hnode = atom_gethash(new_table, (tagged_t)hashcode, str, atom_len);
#else
    hnode = atom_gethash(new_table, (tagged_t)hashcode, str);
#endif
    ciao_atoms = new_table;
    size = size << 1;
  }
  hnode->key = (tagged_t)hashcode;

#if defined(ATOMGC)
    size = size >> 1;     /* atmtab size is one half of ciao_atoms size */
    count = ciao_atoms->next_index;
    while(atmtab[count])                  /* There must be one free entry */
      count =  (count + 1) % size;
    /*ciao_atoms->next_index+1 == size ? 0 : ciao_atoms->next_index+1;*/
    /* next_index should point to a free entry in the table */
    ciao_atoms->next_index = count;
#endif

#if defined(USE_ATOM_LEN)
  hnode->value.atomp = new_atom_check((unsigned char *)str, atom_len, count);
#else
  hnode->value.atomp = new_atom_check((unsigned char *)str, count);
#endif
  atmtab[count] = hnode;

  ciao_atoms->count++;

  INC_MEM_PROG((total_mem_count - current_mem));

  return MakeAtom(count);
}


/* make large object on the heap */
tagged_t make_large(Arg,ptr)
     Argdecl;
     CIAO_REGISTER tagged_t *ptr;
{
  CIAO_REGISTER tagged_t *h = w->global_top;
  tagged_t f = *ptr;
  int ar = LargeArity(f);
  CIAO_REGISTER int i;

  for (i=0; i<ar; i++)
    *h++ = *ptr++;
  *h++ = f;

  w->global_top = h;
  return Tag(STR, h-ar-1);
}

/* Should be non-shared */

/*static tagged_t *numstack_top;*/	                 /* stack pointer */
/*static numstack_t *numstack_first;*/       /* first block in chain */
/*static numstack_t *numstack_last;*/         /* last block in chain */
/*tagged_t *numstack_end;*/	 /* limit of last block, or NULL if undef */

#define NumstackBlockSize(b) ((char *)b->end - (char *)b)

void numstack_init(Arg)
     Argdecl;
{
  int lsize = 1020;

  Numstack_First = (numstack_t *)checkalloc(lsize);
  Numstack_First->next = NULL;
  Numstack_First->end = (tagged_t *)((char *)Numstack_First + lsize);

  Numstack_End = NULL;
}


static void numstack_overflow(Arg)
     Argdecl;
{
  numstack_t *next;

  if (!Numstack_End) {
    while ((next=Numstack_First->next))
      checkdealloc((tagged_t *)Numstack_First,
                   NumstackBlockSize(Numstack_First)),
	Numstack_First = next;

    Numstack_Last = Numstack_First;
  } else {
    int lsize = 2*NumstackBlockSize(Numstack_Last);

    Numstack_Last->next = next = (numstack_t *)checkalloc(lsize);
    next->next = NULL;
    next->end = (tagged_t *)((char *)next + lsize);
    Numstack_Last = next;
  }
  Numstack_Top = (tagged_t *)(Numstack_Last+1);
  Numstack_End = Numstack_Last->end;
}


tagged_t bn_call(Arg, f, x, y, op)
     Argdecl;
     int (*f)();
     tagged_t x, y;
     bcp_t op;
{
  int req, ar;
  CIAO_REGISTER tagged_t **spp, *h;
  tagged_t xx[2], yy[2];
  /*extern bn_from_float();*/

  if (f!=bn_from_float) {
    if (IsFloat(x))
      x = bn_call(Arg,bn_from_float, x, 0, NULL);
    if (IsFloat(y))
      y = bn_call(Arg,bn_from_float, y, 0, NULL);
  }

  if (TagIsSTR(x))
    x = (tagged_t)TagToSTR(x);
  else if (TagIsSmall(x))
    xx[0] = MakeFunctorFix,
    xx[1] = GetSmall(x),
    x = (tagged_t)xx;

  if (TagIsSTR(y))
    y = (tagged_t)TagToSTR(y);
  else if (TagIsSmall(y))
    yy[0] = MakeFunctorFix,
    yy[1] = GetSmall(y),
    y = (tagged_t)yy;

  if (op) {
    spp = &Arg->global_top;
    if ((req=(*f)(x, y, *spp, Heap_End-(*(long *)op)))){
      while (Numstack_Top+req > Numstack_End)
        numstack_overflow(Arg);
      if ((*f)(x, y, Numstack_Top, Numstack_End))
        SERIOUS_FAULT("miscalculated size of bignum");
      explicit_heap_overflow(Arg,req+(*(long *)op), (*(short *)(op+2)));
      if (bn_plus(Numstack_Top, 0, *spp, Heap_End-(*(long *)op)))
        SERIOUS_FAULT("miscalculated size of bignum");
    }
  }
  else {
    spp = &Numstack_Top;
    while (!Numstack_End || (*f)(x, y, Numstack_Top, Numstack_End))
      numstack_overflow(Arg);
  }

  h = *spp;
  ar = LargeArity(h[0]);
  if (ar==2 && IntIsSmall((int)h[1]))
    return MakeSmall(h[1]);
  else
    {
      (*spp) += ar+1;
      h[ar] = h[0];
      return Tag(STR, h);
    }
}

tagged_t make_integer_check(Arg,i, op)
     Argdecl;
     ENG_INT i;
     bcp_t op;
{
  CIAO_REGISTER tagged_t *h;

  if (IntIsSmall(i))
    return MakeSmall(i);

  if (op)
    {				/* compute final value */
      h = w->global_top;
      if (HeapDifference(h, Heap_End) < (*(long *)op)+3)
	explicit_heap_overflow(Arg,(*(long *)op)+3, (*(short *)(op+2))),
	h = w->global_top;

      w->global_top = h+3;
    }
  else
    {				/* compute intermediate value */
      h = Numstack_Top;
      if (h+3 > Numstack_End)
	numstack_overflow(Arg),
	h = Numstack_Top;

      Numstack_Top = h+3;
    }

  HeapPush(h, MakeFunctorFix);
  HeapPush(h, (tagged_t)i);
  HeapPush(h, MakeFunctorFix);
  return Tag(STR, h-3);
}

tagged_t make_float_check(Arg, i, op)
     Argdecl;
     ENG_FLT i;
     bcp_t op;
{
  CIAO_REGISTER tagged_t *h;
  CIAO_REGISTER tagged_t *p = (tagged_t *)(&i);

  if (op)
    {				/* compute final value */
      h = w->global_top;
      if (HeapDifference(h, Heap_End) < (*(long *)op)+4)
	explicit_heap_overflow(Arg,(*(long *)op)+4, (*(short *)(op+2))),
	h = w->global_top;

      w->global_top = h+4;
    }
  else
    {				/* compute intermediate value */
      h = Numstack_Top;
      if (h+4 > Numstack_End)
	numstack_overflow(Arg),
	h = Numstack_Top;

      Numstack_Top = h+4;
    }

  HeapPush(h, MakeFunctorFloat);
  HeapPush(h, p[0]);
  HeapPush(h, p[1]);
  HeapPush(h, MakeFunctorFloat);
  return Tag(STR, h-4);
}


tagged_t make_integer(Arg,i)
     Argdecl;
     ENG_INT i;
{
  CIAO_REGISTER tagged_t *h = w->global_top;

  HeapPush(h, MakeFunctorFix);
  HeapPush(h, (tagged_t)i);
  HeapPush(h, MakeFunctorFix);
  w->global_top = h;
  return Tag(STR, h-3);
}


tagged_t make_float(Arg,i)
     Argdecl;
     ENG_FLT i;
{

  union {
    ENG_FLT i;
    tagged_t p[2];
  } u;
  tagged_t *h;

  h = w->global_top;
  HeapPush(h, MakeFunctorFloat);
  u.i = i;
  HeapPush(h, u.p[0]);
  HeapPush(h, u.p[1]);
  HeapPush(h, MakeFunctorFloat);
  w->global_top = h;
  return Tag(STR, h-4);

}


/* --------------------------------------------------------*/


/* Protect the creation of streams: several threads might want to create
   streams at once. */

extern LOCK stream_list_l;

stream_node_t *insert_new_stream(stream_node_t *new_stream){

  Wait_Acquire_lock(stream_list_l);
  new_stream->forward = root_stream_ptr;
  new_stream->backward = root_stream_ptr->backward;
  root_stream_ptr->backward->forward = new_stream;
  root_stream_ptr->backward = new_stream;
  Release_lock(stream_list_l);

  return new_stream;
}

stream_node_t *new_stream(streamname, streammode, streamfile)
     tagged_t streamname;
     char *streammode;
     FILE *streamfile;
{
  CIAO_REGISTER stream_node_t *s;

  s = (stream_node_t *)checkalloc(sizeof(stream_node_t));
  s->streamname = streamname;
  s->streammode = streammode[0];
  s->pending_char = -100;
  s->socket_eof = FALSE;
  update_stream(s,streamfile);

  return insert_new_stream(s);
}


static int file_is_tty(file)
     FILE *file;
{
  extern int prolog_force_interactive;

  return (isatty(fileno(file)) ||
          (prolog_force_interactive && fileno(file)<3));
}


void update_stream(s,file)
     CIAO_REGISTER stream_node_t *s;
     FILE *file;
{
  s->label = MakeSmall(fileno(file));
  s->streamfile = file;
  if ((s->isatty = file_is_tty(file)))
    s = root_stream_ptr;
  s->last_nl_pos = 0;
  s->nl_count = 0;
  s->char_count = 0;		/* less than perfect */
}


#if defined(CREATE_NEW_STREAMS)
void update_std_streams()		/* called by restore/1 */
{
  struct
    stream_node *streamptr = root_stream_ptr, *next_ptr;

  do {
    next_ptr = streamptr->forward;
    if (streamptr->streamname!=ERRORTAG)
      fclose(streamptr->streamfile);
    checkdealloc(streamptr);
    streamptr = next_ptr;
  } while (streamptr!=root_stream_ptr);
  init_streams();
  init_streams_each_time(Arg);
}
#else
void update_std_streams()		/* called by restore/1 */
{
  stream_node_t *streamptr = root_stream_ptr->forward;

  while (streamptr!=root_stream_ptr) {         /* close any ghost streams */
    if (streamptr->streamname!=ERRORTAG)
      fclose(streamptr->streamfile);
    else			      /* check if std stream is a tty now */
      update_stream(streamptr,streamptr->streamfile);
    streamptr = streamptr->forward;
  }
}
#endif

/*-------------------------------------------------------*/

/* Inserts the definition of a predicate, either asserted, compiled,
   consulted, or qloaded. */

definition_t *new_functor(tagpname,arity)
     tagged_t tagpname;
     int arity;
{
  CIAO_REGISTER definition_t *func;
  CIAO_REGISTER int i;

  /* How to get the printable name (i.e., accessing to the atom part):

  if ((tagpname & 3) == 0)
    printf("New predicate head %s, arity %d\n", GetString(tagpname), arity);
    */

  func = (definition_t *)checkalloc(sizeof(definition_t));
  for (i=0; i<sizeof(definition_t); i++)
    ((char *)func)[i] = 0;
  func->printname = tagpname;
  func->arity = arity;
  SetEnterInstr(func, ENTER_UNDEFINED);
  func->code.undinfo = NULL;
  return func;
}


/*------------------------------------------------------------*/

void expand_sw_on_key(psw,otherwise,deletep)
     sw_on_key_t **psw;
     try_node_t *otherwise;
     bool_t deletep;
{
  CIAO_REGISTER sw_on_key_node_t *h1, *h2;
  int size = SwitchSize(*psw);
  sw_on_key_t *newsw = new_switch_on_key(size<<1,otherwise);
  CIAO_REGISTER int j;

  for (j=size-1; j>=0; --j) {
    h1 = &(*psw)->tab.asnode[j];
    if (h1->key) {
      newsw->count++;
      h2 = incore_gethash(newsw,h1->key);
      h2->key = h1->key;
      h2->value.try_chain = h1->value.try_chain;
    }
  }

  if (deletep) {
    checkdealloc((tagged_t *)(*psw),sizeof(sw_on_key_t)+
		 (size-ANY)*sizeof(sw_on_key_node_t));
  } else leave_to_gc(TABLE, (char *)(*psw));

  (*psw) = newsw;
}


void add_definition(swp,node,key,def)
     sw_on_key_t **swp;
     sw_on_key_node_t *node;
     tagged_t key;
     definition_t *def;
{
  node->key=key;
  node->value.def=def;
  if (((*swp)->count+=1)<<1 > SwitchSize(*swp))
    expand_sw_on_key(swp,NULL,FALSE);
}


definition_t *insert_definition(swp,tagpname,arity,insertp)
     sw_on_key_t **swp;
     tagged_t tagpname;
     int arity;
     bool_t insertp;
{
  CIAO_REGISTER sw_on_key_node_t *keyval;
  definition_t *value = NULL;
  tagged_t key=SetArity(tagpname,arity);

  /* Lock here -- we do not want two different workers to add predicates
     concurrently. */

  Wait_Acquire_slock(prolog_predicates_l);
  keyval = (sw_on_key_node_t *)incore_gethash((*swp),key);

  if (keyval->key)                                    /* Already existent */
    value = keyval->value.def;
  else if (insertp){                                      /* New predicate */
    value=new_functor(tagpname, arity);
    add_definition(swp, keyval, key, value);
  }

  Release_slock(prolog_predicates_l);

  return value;
}





/*------------------------------------------------------------*/

/* Create a most general term for a given functor or small int. */
tagged_t make_structure(Arg,functor)
     Argdecl;
     CIAO_REGISTER tagged_t functor;
{
  CIAO_REGISTER int ar = Arity(functor);
  CIAO_REGISTER tagged_t *h = w->global_top;

  if (ar==0 || !TagIsATM(functor))
    return functor;
  else if (functor==functor_list) {
    ConstrHVA(h);
    ConstrHVA(h);
    w->global_top = h;
    return Tag(LST,HeapOffset(h,-2));
  } else {
    HeapPush(h,functor);
    do {
      ConstrHVA(h);
    } while (--ar);
    w->global_top = h;

    return Tag(STR,h-Arity(functor)-1);
  }
}

definition_t *find_definition(swp,term,argl,insertp)
     sw_on_key_t **swp;
     tagged_t term,**argl;
     bool_t insertp;
{
  int arity;

  if (TagIsStructure(term)) {
    tagged_t f = TagToHeadfunctor(term);

    *argl = TagToArg(term,1);
    term = SetArity(f,0);
    arity = Arity(f);
  } else
    if (TagIsLST(term)) {
      *argl = TagToLST(term);
      term = atom_list;
      arity = 2;
    }
    else
      arity = 0;

  return insert_definition(swp,term,arity,insertp);
}


static definition_t *parse_1_definition();

/* Enter here with a *definition name* as generated by the compiler. */
definition_t *parse_definition(complex)
     tagged_t complex;
{
  tagged_t a,b;

  if (TagIsSTR(complex) && (TagToHeadfunctor(complex)==functor_slash)) {
    DerefArg(a,complex,1);
    DerefArg(b,complex,2);
    return parse_1_definition(a,b);
  }
  else return NULL;
}

static definition_t **find_subdef_chain(f, clause_no)
     definition_t *f;
     int clause_no;
{
  incore_info_t *d = f->code.incoreinfo;
  CIAO_REGISTER emul_info_t *ep;

  if (clause_no && clause_no <= *(int *)d->clauses_tail)
    for (ep = d->clauses; --clause_no; ep = ep->next)
      ;
  else {
    ep = (emul_info_t *)d->clauses_tail;
    if (ep->objsize || ep->next == NULL) {
      ep = (emul_info_t *)checkalloc(sizeof(emul_info_t));
      ep->subdefs = NULL;
      ep->objsize = 0;
      ep->next = (emul_info_t *)(*(int *)d->clauses_tail + 1);
      *d->clauses_tail = ep;
      d->clauses_tail = &ep->next;
    }
  }
  return &ep->subdefs;
}


static definition_t *parse_1_definition(tagname, tagarity)
     tagged_t tagname,tagarity;
{
  int arity;

  if (!TagIsSmall(tagarity))
    return NULL;
  arity = GetSmall(tagarity);
  if (TagIsSTR(tagname) && (TagToHeadfunctor(tagname)==functor_minus))
    /* "internal" predicate */
    {
      definition_t *f, *f1, **pf;
      tagged_t tmp;
      int i, subdef_no, clause_no;

      DerefArg(tmp,tagname,2);
      subdef_no = GetSmall(tmp);
      DerefArg(tagname,tagname,1);

      DerefArg(tmp,tagname,2);
      clause_no = GetSmall(tmp);
      DerefArg(tagname,tagname,1);

      f = parse_definition(tagname);
      if (f==NULL)
	return NULL;
      i = f->predtyp;
      if (i > ENTER_FASTCODE_INDEXED)
	return NULL;
      pf = find_subdef_chain(f, clause_no);

      if (!(*pf))
	f = *pf = new_functor((tagged_t)f|3, arity);
      else
	{
	  for (i=1, f1 = *pf;
	       !(f1->printname&2);
	       i++, f1 = (definition_t *)TagToPointer(f1->printname))
	    if (i==subdef_no) break;
	
	  if (i==subdef_no) return f1;
	  f1->printname = (tagged_t)(f=new_functor(f1->printname, arity))|1;
	}
      return f;
    }

  if (TagIsATM(tagname)) 
    return insert_definition(predicates_location,tagname,arity,TRUE);
  else return NULL;
}

static bool_t cunify_args_aux PROTO((worker_t *w, int arity, tagged_t *pt1, tagged_t *pt2, tagged_t *x1, tagged_t *x2));
static bool_t cunify_aux PROTO((worker_t *w, tagged_t x1, tagged_t x2));

/* Unify the argument lists of two compund terms.
 * pt1 - first argument list.
 * pt2 - second argument list.
 * arity - number of arguments.
 */
bool_t cunify_args(Arg,arity,pt1,pt2)
     Argdecl;
     CIAO_REGISTER int arity;
     CIAO_REGISTER tagged_t *pt1, *pt2;
{
  tagged_t x1, x2;
  bool_t result =
    (cunify_args_aux(Arg,arity,pt1,pt2,&x1,&x2) && cunify_aux(Arg,x1,x2));
  CIAO_REGISTER int i = w->value_trail;

  if (i<InitialValueTrail) {
    pt2 = (tagged_t *)w->node;
    do {
      pt1 = (tagged_t *)pt2[i++];
      *pt1 = pt2[i++];
    } while (i<InitialValueTrail);
    w->value_trail = InitialValueTrail;
  }

  return result;
}


static bool_t cunify_args_aux(Arg,arity,pt1,pt2,x1,x2)
     Argdecl;
     CIAO_REGISTER int arity;
     CIAO_REGISTER tagged_t *pt1, *pt2;
     tagged_t *x1, *x2;
{
  CIAO_REGISTER tagged_t 
    t1 = ~0,
    t2 = ~0,
    t3;

  /* Terminating unification of complex structures: Forward args of pt2 to
     args of pt1 using choice stack as value cell.  When done, reinstall
     values. */

  if (ChoiceYounger(ChoiceOffset(w->node,2*CHOICEPAD-w->value_trail),w->trail_top))
				/* really: < 2*arity */
    choice_overflow(Arg,2*CHOICEPAD);
  for (; arity>0; --arity) {
    t1 = *pt1, t2 = *pt2;
    if (t1 != t2) {
      DerefHeapSwitch(t1,t3,goto noforward;);
      DerefHeapSwitch(t2,t3,goto noforward;);
      if (t1!=t2 && IsComplex(t1&t2)) {
        /* replace smaller value by larger value,
           using choice stack as value trail */
        CIAO_REGISTER tagged_t *b = (tagged_t *)w->node;
        CIAO_REGISTER int i = w->value_trail;

        if (t1>t2)
          b[--i] = *pt1,
            b[--i] = (tagged_t)pt1,
            *pt1 = t2;
        else
          b[--i] = *pt2,
            b[--i] = (tagged_t)pt2,
            *pt2 = t1;
        w->value_trail = i;
      noforward:
        if (arity>1 && !cunify_aux(Arg,t1,t2))
          return FALSE;
      } else if (t1 != t2)
        return FALSE;
    }
    (void)HeapNext(pt1);
    (void)HeapNext(pt2);
  }

  *x1 = t1, *x2 = t2;

  if (ChoiceYounger(ChoiceOffset(w->node,CHOICEPAD-w->value_trail),w->trail_top))
    choice_overflow(Arg,CHOICEPAD);
  return TRUE;
}


/* Unify two terms.
 * x1 - first term
 * x2 - second term
 */
bool_t cunify(Arg,x1,x2)
     Argdecl;
     tagged_t x1, x2;
{
  bool_t result = cunify_aux(Arg,x1,x2);
  CIAO_REGISTER int i = w->value_trail;

  if (i<InitialValueTrail) {
    CIAO_REGISTER tagged_t *pt1, *pt2;

    pt2 = (tagged_t *)w->node;
    do {
      pt1 = (tagged_t *)pt2[i++];
      *pt1 = pt2[i++];
    } while (i<InitialValueTrail);
    w->value_trail = InitialValueTrail;
  }

  return result;
}


static bool_t cunify_aux(Arg,x1,x2)
     Argdecl;
     tagged_t x1, x2;
{
  CIAO_REGISTER tagged_t u, v, t1;

 in:
  u=x1, v=x2;

  SwitchOnVar(u,t1,
	      {goto u_is_hva;},
	      {goto u_is_cva;},
	      {goto u_is_sva;},
	      ;);

				/* one non variable */
  SwitchOnVar(v,t1,
	      { BindHVA(v,u); goto win; },
	      { BindCVA(v,u); Wake; goto win; },
	      { BindSVA(v,u); goto win; },
	      ;);

				/* two non variables */
  if (!(v ^= u))		/* are they equal? */
    goto win;
  else if (v>=QMask)		/* not the same type? */
    goto lose;
  else if (!(u & TagBitComplex)) /* atomic? (& not LNUM)*/
    goto lose;
  else if (!(u & TagBitFunctor)) /* list? */
    {
      v ^= u;			/* restore v */
      if (cunify_args_aux(Arg,2,TagToCar(u),TagToCar(v),&x1,&x2))
	goto in;
      else
	goto lose;
    }
  else				/* structure. */
    {
      v ^= u;			/* restore v */
      if (TagToHeadfunctor(u) != (t1=TagToHeadfunctor(v)))
	goto lose;
      else if (t1&QMask)	/* large number */
	{
	  int i;
	
	  for (i = LargeArity(t1)-1; i>0; i--)
	    if (CTagToArg(u,i) != CTagToArg(v,i)) goto lose;
	  goto win;
	}
      if (cunify_args_aux(Arg,Arity(t1),TagToArg(u,1),TagToArg(v,1),&x1,&x2))
	goto in;
      else
	goto lose;
    }

 u_is_hva:
  SwitchOnVar(v,t1,
	      { if (u==v)
		  ;
		else if (YoungerHeapVar(TagToHVA(v),TagToHVA(u)))
		  BindHVA(v,u)
		else
		  BindHVA(u,v); },
	      { BindHVA(u,v); },
	      { BindSVA(v,u); },
	      { BindHVA(u,v); });
  goto win;

 u_is_cva:
  SwitchOnVar(v,t1,
	      { BindHVA(v,u); },
	      { if (u==v)
		  ;
		else if (YoungerHeapVar(TagToCVA(v),TagToCVA(u)))
		  { BindCVA(v,u); Wake; }
		else
		  { BindCVA(u,v); Wake; } },
	      { BindSVA(v,u); },
	      { BindCVA(u,v); Wake; });
  goto win;

 u_is_sva:
  for (; TagIsSVA(v); v = t1)
    {
      RefSVA(t1,v);
      if (v == t1)
	{
	  if (u==v)
	    ;
	  else if (YoungerStackVar(TagToSVA(v),TagToSVA(u)))
	    BindSVA(v,u)
	  else
	    BindSVA(u,v);
	  goto win;
	}
    }
  BindSVA(u,v);

 win:
  return TRUE;

 lose:
  return FALSE;
}

/* --------------------------------------------------------------------------- */
/* instance */
static bool_t cinstance_args_aux PROTO((worker_t *w, int arity, tagged_t *pt1, tagged_t *pt2, tagged_t *x1, tagged_t *x2, int *n));
static bool_t cinstance_aux PROTO((worker_t *w, tagged_t x1, tagged_t x2, int *n));

bool_t cinstance_args(Arg,arity,pt1,pt2,n)
     Argdecl;
     CIAO_REGISTER int arity;
     CIAO_REGISTER tagged_t *pt1, *pt2;
     int *n;
{
  tagged_t x1, x2;
  return cinstance_args_aux(Arg,arity,pt1,pt2,&x1,&x2,n) && cinstance_aux(Arg,x1,x2,n);
}

static bool_t cinstance_args_aux(Arg,arity,pt1,pt2,x1,x2,n)
     Argdecl;
     CIAO_REGISTER int arity;
     CIAO_REGISTER tagged_t *pt1, *pt2;
     tagged_t *x1, *x2;
     int *n;
{
  CIAO_REGISTER tagged_t 
    t1 = ~0,
    t2 = ~0;

  /* Terminating unification of complex structures: Forward args of pt2 to
     args of pt1 using choice stack as value cell.  When done, reinstall
     values. */

  if (ChoiceYounger(ChoiceOffset(w->node,2*CHOICEPAD),w->trail_top))
				/* really: < 2*arity */
    choice_overflow(Arg,2*CHOICEPAD);
  for (; arity>0; --arity) {
    t1 = *pt1, t2 = *pt2;
    if (arity>1 && !cinstance_aux(Arg,t1,t2,n))
      return FALSE;
    (void)HeapNext(pt1);
    (void)HeapNext(pt2);
  }

  *x1 = t1, *x2 = t2;

  if (ChoiceYounger(ChoiceOffset(w->node,CHOICEPAD),w->trail_top))
    choice_overflow(Arg,CHOICEPAD);
  return TRUE;
}

void pop_choicept(Argdecl);
void push_choicept(Argdecl, try_node_t *alt);

bool_t cinstance(Arg)
     Argdecl;
{
  tagged_t t1, t2, *pt1, *pt2;
  int result;
  int n = 0;

  t1 = X(0);
  t2 = X(1);

  push_choicept(Arg,fail_alt);	/* try, arity=0 */

  result = cinstance_aux(Arg,X(0),X(1),&n);

  pt1 = pt2 = TagToPointer(w->node->trail_top); /* untrail */
  while (!OffTrailtop(pt2,w->trail_top)) {
    t1 = TrailNext(pt2);	/* old var */
    CTagToPointer(t1) = t1;
  }
  w->trail_top = pt1;

  pop_choicept(Arg);		/* trust */

  return result;
}

static bool_t cinstance_aux(Arg,x1,x2,n)
     Argdecl;
     tagged_t x1, x2;
     int *n;
{
  CIAO_REGISTER tagged_t u, v, t1, nt;

 in:
  u=x1, v=x2;

  nt = MakeSmall(*n);
  SwitchOnVar(u,t1,
	      { goto u_is_hva; },
	      { goto lose; }, /* CVAs are not supported */
	      { goto u_is_sva; },
	      { goto one_non_var; });
  /* note that if deref(u) == deref(v), the following code must do nothing */
 u_is_hva:
  SwitchOnVar(v,t1,
              { BindHVA(v, nt); },
              { goto lose; }, /* CVAs are not supported */
	      { BindSVA(v, nt); },
	      { goto lose; });
  if (u != v) BindHVA(u, nt);
  goto var_win;

 u_is_sva:
  SwitchOnVar(v,t1,
              { BindHVA(v, nt); },
              { goto lose; }, /* CVAs are not supported */
	      { BindSVA(v, nt); },
	      { goto lose; });
  if (u != v) BindSVA(u, nt);
  goto var_win;

 var_win: 
  (*n)++;
  goto win;

 one_non_var:
				/* one non variable */
  SwitchOnVar(v,t1,
	      { BindHVA(v,u); goto win; },
	      { BindCVA(v,u); Wake; goto win; },
	      { BindSVA(v,u); goto win; },
	      ;);

				/* two non variables */
  if (!(v ^= u))		/* are they equal? */
    goto win;
  else if (v>=QMask)		/* not the same type? */
    goto lose;
  else if (!(u & TagBitComplex)) /* atomic? (& not LNUM)*/
    goto lose;
  else if (!(u & TagBitFunctor)) /* list? */
    {
      v ^= u;			/* restore v */
      if (cinstance_args_aux(Arg,2,TagToCar(u),TagToCar(v),&x1,&x2,n))
	goto in;
      else
	goto lose;
    }
  else				/* structure. */
    {
      v ^= u;			/* restore v */
      if (TagToHeadfunctor(u) != (t1=TagToHeadfunctor(v)))
	goto lose;
      else if (t1&QMask)	/* large number */
	{
	  int i;
	
	  for (i = LargeArity(t1)-1; i>0; i--)
	    if (CTagToArg(u,i) != CTagToArg(v,i)) goto lose;
	  goto win;
	}
      if (cinstance_args_aux(Arg,Arity(t1),TagToArg(u,1),TagToArg(v,1),&x1,&x2,n))
	goto in;
      else
	goto lose;
    }

 win:
  return TRUE;

 lose:
  return FALSE;
}

/* --------------------------------------------------------------------------- */
/* ground */
static bool_t cground_args_aux PROTO((worker_t *w, int arity, tagged_t *pt1, tagged_t *x1));
static bool_t cground_aux PROTO((worker_t *w, tagged_t x1));

static bool_t cground_args_aux(Arg,arity,pt1,x1)
     Argdecl;
     CIAO_REGISTER int arity;
     CIAO_REGISTER tagged_t *pt1;
     tagged_t *x1;
{
  CIAO_REGISTER tagged_t 
    t1 = ~0;
  for (; arity>0; --arity) {
    t1 = *pt1;
    if (arity > 1 && !cground_aux(Arg,t1)) return FALSE;
    (void)HeapNext(pt1);
  }
  *x1 = t1;
  return TRUE;
}

bool_t cground(Arg)
     Argdecl;
{
  return cground_aux(Arg,X(0));
}

static bool_t cground_aux(Arg,x1)
     Argdecl;
     tagged_t x1;
{
  CIAO_REGISTER tagged_t u, t1;

 in:
  u=x1;

  SwitchOnVar(u,t1,
	      { goto lose; },
	      { goto lose; }, /* CVAs are not supported */
	      { goto lose; },
	      { goto non_var; });

 non_var:
  if (TagIsATM(u)) goto win;
  if (TagIsSmall(u)) goto win;
  if (TagIsLST(u)) {
      if (cground_args_aux(Arg,2,TagToCar(u),&x1))
	goto in;
      else
	goto lose;
    }
  else				/* structure. */
    {
      t1=TagToHeadfunctor(u);
      if (t1&QMask)	/* large number */
	{
	  goto win;
	}
      if (cground_args_aux(Arg,Arity(t1),TagToArg(u,1),&x1))
	goto in;
      else
	goto lose;
    }

 lose:
  return FALSE;
 win:
  return TRUE;
}

/* --------------------------------------------------------------------------- */

ENG_INT get_integer(t)
     CIAO_REGISTER tagged_t t;
{
  /* small int's taken care of by GetInteger() */

  if (LargeIsFloat(t))
    return get_float(t);
  else
    return (ENG_INT)CTagToArg(t,1);
}

ENG_FLT get_float(t)
     CIAO_REGISTER tagged_t t;
{
  /* small int's taken care of by GetFloat() */

  if (!LargeIsFloat(t)) {
    CIAO_REGISTER int /* i, */ /* unused */
      ar = LargeArity(TagToHeadfunctor(t))-1;
    ENG_FLT f = (ENG_INT)CTagToArg(t,ar);

    while (ar>1) {
      unsigned long u = CTagToArg(t,--ar);

      if (u & 0x80000000)	/* trouble on some machines */
        f = f*4294967296.0 + 2147483648.0 + (u - 0x80000000);
      else
        f = f*4294967296.0 + u;
    }

    return f;
  }

  {
    ENG_FLT f;
    CIAO_REGISTER tagged_t *p = (tagged_t *)(&f);

    p[0] = CTagToArg(t,1);
    p[1] = CTagToArg(t,2);
    return f;
  }
}

bool_t float_is_finite(t)
     CIAO_REGISTER tagged_t t;
{
  /* Assume IEEE comparison for floats */

  ENG_FLT f = get_float(t);
  f = f - f;
  if (f == f) return TRUE;
  else return FALSE;
}

/* '$stream'(<address>,<id>) <-- (stream_node_t *) */

tagged_t ptr_to_stream_noalias(Arg, n)
     Argdecl;
     CIAO_REGISTER stream_node_t *n;
{
  CIAO_REGISTER tagged_t *pt1 = w->global_top;

  /*
  printf("(int)n is %ud\n", (int)n);
  printf("n->label is %ud\n", n->label);
  */

  HeapPush(pt1,functor_Dstream);
  HeapPush(pt1,PointerToTerm(n));
  HeapPush(pt1,n->label);
  return (w->global_top=pt1, Tag(STR,HeapOffset(pt1,-3)));
}


/* '$stream'(<address>,<id>) <-- (stream_node_t *) */
/* or */
/* <stream_alias> <-- (stream_node_t *) */
tagged_t ptr_to_stream(Arg,n)
     Argdecl;
     CIAO_REGISTER stream_node_t *n;
{
  if (n==stream_user_input)
    return atom_user_input;
  if (n==stream_user_output)
    return atom_user_output;
  if (n==stream_user_error)
    return atom_user_error;

  return ptr_to_stream_noalias(Arg, n);
}


/* '$stream'(<address>,<id>) --> (stream_node_t *)
                          --> NULL, if invalid
   'r' - read mode,  streammode=[rs]
   'w' - write mode, streammode=[was]
   'x' - any mode,   streammode=[rwas]
   'y' - any mode but no standard streams  */
stream_node_t *stream_to_ptr(t, mode)
     CIAO_REGISTER tagged_t t;
     int mode;
{
  CIAO_REGISTER stream_node_t *n = NULL;
  CIAO_REGISTER tagged_t x1, x2;

  DerefSwitch(t,x1,;);

  if (TagIsATM(t))
    {
      if (mode=='y')
	n = NULL;
      else if (t==atom_user)
	n = (mode=='r' ? stream_user_input : stream_user_output);
      else if (t==atom_user_input)
	n = stream_user_input;
      else if (t==atom_user_output)
	n = stream_user_output;
      else if (t==atom_user_error)
	n = stream_user_error;
    }
  else if (TagIsSTR(t) && (TagToHeadfunctor(t) == functor_Dstream))
    {
      DerefArg(x1,t,1);
      DerefArg(x2,t,2);
      if (!TagIsSmall(x1) || !TagIsSmall(x2) ||
	  (n=TagToStream(x1), n->label != x2))
	n = NULL;
    }

  if (((mode=='r') && (n!=NULL) && (n->streammode=='w'||n->streammode=='a')) ||
      ((mode=='w') && (n!=NULL) && (n->streammode=='r')))
    return NULL;
  else
    return n;
}

/* Similar to stream_to_ptr(), but giving an error code */

stream_node_t *stream_to_ptr_check(t, mode, errcode)
     CIAO_REGISTER tagged_t t;
     int mode; /* not 'y' */
     int *errcode;
{
  CIAO_REGISTER stream_node_t *n = NULL;
  CIAO_REGISTER tagged_t x1, x2;

  DerefSwitch(t,x1,{*errcode = INSTANTIATION_ERROR; return NULL;});

  if (TagIsATM(t))
    {
      if (t==atom_user)
	n = (mode=='r' ? stream_user_input : stream_user_output);
      else if (t==atom_user_input)
	n = stream_user_input;
      else if (t==atom_user_output)
	n = stream_user_output;
      else if (t==atom_user_error)
	n = stream_user_error;
      else {
            *errcode = DOMAIN_ERROR(STREAM_OR_ALIAS);
            return NULL;
      }
    }
  else if (TagIsSTR(t) && (TagToHeadfunctor(t) == functor_Dstream)) {
      DerefArg(x1,t,1);
      DerefArg(x2,t,2);
      if (!TagIsSmall(x1) || !TagIsSmall(x2) ||
	  (n = TagToStream(x1), n->label != x2)) {
            *errcode = EXISTENCE_ERROR(STREAM);
            return NULL;
      }
    } else {
        *errcode = DOMAIN_ERROR(STREAM_OR_ALIAS);
        return NULL;
    }

  if (mode=='r') {
    if (n->streammode=='w'||n->streammode=='a') {
      *errcode = PERMISSION_ERROR(ACCESS,STREAM);  
      return NULL;
    }
  } else if (mode=='w') {
    if (n->streammode=='r') {
      *errcode = PERMISSION_ERROR(MODIFY,STREAM); 
      return NULL;
    }
  }

  return n;
}

bool_t prolog_show_nodes(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  show_nodes(w, ChoiceFromInt(X(0)), ChoiceFromInt(X(1)));
  return TRUE;
}

bool_t prolog_show_all_nodes(Arg)
     Argdecl;
{
  show_nodes(w, w->node, InitialNode);
  return TRUE;
}


bool_t start_node(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  Unify_constant(ChoiceToInt(InitialNode),X(0));
  return TRUE;
}

#if defined(DEBUG_NODE)
void display_functor(definition_t *functor)
{
  if(functor)
    {
      if(IsString((functor)->printname))
	{
	  fprintf(stderr, "'%s'/", GetString((functor)->printname));
	}
      else
	{
	  fprintf(stderr, "_/");
	}
      fprintf(stderr, "%d", (functor)->arity);
    }
  else
    fprintf(stderr, "_F");
}
# define DisplayCPFunctor(cp) display_functor(cp->functor)
#else
# define DisplayCPFunctor(cp) fprintf(stderr, "_N")
#endif

void show_nodes(Argdecl, node_t *cp_younger, node_t *cp_older)
{
  int number;
  try_node_t *next_alt;
#if !defined(DEBUG_NODE)
  fprintf(stderr, "/* functor information in nodes not available */\n");
#endif
  fprintf(stderr, "nodes(");
  fprintf(stderr,"0x%08lx:",(unsigned long)cp_younger);
  DisplayCPFunctor(cp_younger);
  fprintf(stderr, ", ");
  fprintf(stderr, "[");
  if (cp_younger->next_alt)
    next_alt = cp_younger->next_alt;
  else
    next_alt = w->next_alt;
  number = next_alt->number;
  cp_younger = ChoiceCharOffset(cp_younger, -next_alt->node_offset);
  while(ChoiceYounger(cp_younger, cp_older)) {
    fprintf(stderr,"\n  ");
    fprintf(stderr,"0x%08lx:",(unsigned long)cp_younger);
    DisplayCPFunctor(cp_younger);
    fprintf(stderr, "/%d,", number);
    number = cp_younger->next_alt->number;
    cp_younger = ChoiceCharOffset(cp_younger,
				  -cp_younger->next_alt->node_offset);
  }
  if (!ChoiceYounger(cp_older, cp_younger)) {
    fprintf(stderr,"\n  ");
    fprintf(stderr,"0x%08lx:",(unsigned long)cp_older);
    DisplayCPFunctor(cp_older);
    fprintf(stderr, "/%d\n",number);
  }
  fprintf(stderr, "])\n");
}
