/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include "cont_calls.h"
#include "tries.c"

/* -------------------------- */
/*     Inline Procedures      */
/* -------------------------- */

tagged_t *ini;

ENGINE_Term my_make_var(goal_descriptor_t *state)
{
  ciao_ensure_heap(state, 1);
  worker_t * w = state->worker_registers;
  ENGINE_Term *pt = w->global_top;
  ENGINE_Term resul = TagHVA(pt);
  HeapPush(pt, resul);
  w->global_top = pt;
  return resul;
}			      

ENGINE_Term my_make_integer(goal_descriptor_t *state, int i) 
{
  ciao_ensure_heap(state, 4);
  worker_t * w = state->worker_registers;
  return MakeInteger(w, i);
}

ENGINE_Term my_make_float(goal_descriptor_t *state, double f) 
{
  ciao_ensure_heap(state, 4);
  worker_t * w = state->worker_registers;
  return MakeFloat(w, f);
}

ENGINE_Term my_make_list(goal_descriptor_t *state, ENGINE_Term head, ENGINE_Term tail) 
{
  ENGINE_Term list;
  ciao_ensure_heap(state, 3);
  worker_t * w = state->worker_registers;
  MakeLST(list, head, tail);
  return list;
}

ENGINE_Term my_make_functor(goal_descriptor_t *state, char* name, int arity, ENGINE_Term *args) 
{
  if (arity == 0) return MakeString((char *)name);
  else if (strcmp(name, ".") == 0 && arity == 2) 
    {
      ENGINE_Term list;
      ciao_ensure_heap(state, 3);
      worker_t * w = state->worker_registers;
      MakeLST(list, args[0], args[1]);
      return list;
    } 
  else 
    {
      int i;
      ENGINE_Term *pt;
      ENGINE_Term functor;
      ciao_ensure_heap(state, 2 + arity);
      functor = SetArity(MakeString((char *)name), arity);
      worker_t * w = state->worker_registers;
      pt = w->global_top;
      HeapPush(pt, functor);
      for (i = 0; i < arity; i++) HeapPush(pt, args[i]);
      w->global_top = pt;  
      return Tag(STR, HeapOffset(pt, -(arity+1)));
    }
}

struct EACH_CALL *handlecall(ENGINE_Term Call) 
{	     
  struct EACH_CALL *callid;
  
  TrNode node = put_trie_entry(node_top,Call,MODE_STANDARD);
  
  if (node->child == NULL) 
    {
      node->child = (trie_node_t *)mem_alloc(sizeof(struct EACH_CALL));
      callid = (struct EACH_CALL *)(node->child);
      
      callid->last_answer = callid->answers;
      callid->last_cont = callid->conts;

      callid->ans_cnt = 0; //No answers

      if(TOP_SF==NULL) //First tabling predicate
	{
	  callid->dfn = 0;
	  callid->plink = 0;
	} 
      else //Seconds tabling predicates
	{
	  callid->dfn = ((struct EACH_CALL *)TOP_SF)->dfn + 1;
	  callid->plink = ((struct EACH_CALL *)TOP_SF)->dfn + 1;	
	}
      
      callid->previous = TOP_SF;
      TOP_SF = callid;
      callid->comp = READY;
      callid->trie_answers=open_trie();
      callid->node_first=NULL;
      callid->node_last=NULL;
      return callid;
    }
  else return (struct EACH_CALL *)(node->child);
}

int complete (struct EACH_CALL *call, struct EACH_CALL *parentcallid) 
{
  struct EACH_CALL *ccall;

  if (call->plink == call->dfn) 
    {
      do 
	{
	  if ((long)TOP_SF == (long)call) break;
	  ccall = (struct EACH_CALL *)TOP_SF;
	  ccall->comp = COMPLETE;
	  //DELE_AND_FREE_WAITS(ccall->waits);
	  //Liberar estructuras
	  TOP_SF = TOP_SF->previous;
	} while ((long)TOP_SF != (long)call);
      call->comp=COMPLETE;	    
      //DELE_AND_FREE_WAITS(call->waits);	
      //Liberar estructuras
      TOP_SF = TOP_SF->previous;    
    } 
  else SETMIN(parentcallid->plink,call->plink);

  return TRUE;
}

void freeze_heap(node_t *ind, ENGINE_Term *heap_top)
{
  do
    {
      ind->global_top = sharedGtop;
      ind = ChoiceCharOffset(ind,-ind->next_alt->node_offset);
    }
  while (ind->global_top != sharedGtop);

  *sharedGtop = (ENGINE_Term) heap_top;
}

struct untrail_list *get_free_vars(struct untrail_list *l, ENGINE_Term t)
{
  DEREF(t,t);
  if (IsFreeVar(t))
    {
      struct untrail_list *newL = (struct untrail_list*) mem_alloc (sizeof(struct untrail_list));
      newL->next = l;
      newL->value = t;
      return newL;
    }
 
  if (ENGINE_IsPairTerm(t))
    {
      l = get_free_vars(l, ENGINE_HeadOfTerm(t));
      l = get_free_vars(l, ENGINE_TailOfTerm(t));
      return l;
    }

  if (ENGINE_IsApplTerm(t))
  {
    int i, arity = ENGINE_ArityOfFunctor(t);
    for (i = 1; i <= arity; i++)
      {
	ENGINE_Term arg = ENGINE_ArgOfTerm(i,t);
	DEREF(arg,arg);
	if (IsFreeVar(arg))
	  {
	    struct untrail_list *newL = (struct untrail_list*) mem_alloc (sizeof(struct untrail_list));
	    newL->next = l;
	    newL->value = arg;
	    l = newL;
	  }

	if (ENGINE_IsPairTerm(arg))
	  {
	    l = get_free_vars(l, ENGINE_HeadOfTerm(arg));
	    l = get_free_vars(l, ENGINE_TailOfTerm(arg));
	  }

	if (ENGINE_IsApplTerm(arg)) 
	    l = get_free_vars(l,arg);

      }
  }
  return l;
}

struct untrail_list *get_binds(ENGINE_Term freeVars)
{
  struct untrail_list *list = (struct untrail_list *)ENGINE_IntOfTerm(freeVars);
  if (list == NULL) return (struct untrail_list*)  MallocBase;
  struct untrail_list *resul = (struct untrail_list *) checkalloc (sizeof(struct untrail_list));
  struct untrail_list *aux = resul;

  if (list != NULL)
    {
      aux->value = CTagToPointer(list->value);
      while (list->next != NULL)
	{
	  list = list->next;
	  aux->next = (struct untrail_list *) mem_alloc (sizeof(struct untrail_list));
	  aux = aux->next;
	  aux->value = CTagToPointer(list->value);
	}
      aux->next = NULL;
    }

  return resul;
}

void put_binds(ENGINE_Term links, struct untrail_list *binds)
{
  if ((int)binds == MallocBase) return;
  struct untrail_list *list = (struct untrail_list *)ENGINE_IntOfTerm(links);

  while (list != NULL)
    {
      CTagToPointer(list->value) = binds->value;
      list = list->next;
      binds = binds->next;
    }
}


void make_ground(Argdecl, ENGINE_Term *index, ENGINE_Term *indexFin, ENGINE_Term *pt)
{
  ENGINE_Term *i_term;
  int i, arity;
  ENGINE_Term *ptIni = pt;

  while(index < pt)
    {
      ENGINE_Term t = *index;
      if (t == 0xb0000008) index = index + 3;
      else if (IsVar(t))
	{
	  DEREF(t,t);
	  if (IsVar(t)) HeapPush(index, (unsigned int)index - MallocBase); //METER BINDS ENTRE EL TERMINO!!!
	  else *index = t;	  
	}
      else if (TagIsLST(t))
	{
	  HeapPush(index, 0xc0000000 + ((unsigned int)pt - MallocBase));
	  ENGINE_Term *list = TagToPointer(t);
	  HeapPush(pt, *list);
	  list++;
	  HeapPush(pt, *list);
	}
      else if (TagIsSTR(t))
	{
	  if (IsNumber(t)) index++; 
	  else
	    {
	      HeapPush(index, 0xe0000000 + ((unsigned int)pt - MallocBase));
	      i_term = TagToPointer(t);
	      arity = Arity(TagToHeadfunctor(t));

	      for (i = 0; i <= arity; i++)
		{
		  HeapPush(pt, *i_term);
		  i_term++;
		}
	    }
	}
      else index++;

      if (index == indexFin) index = ptIni;
    }

  Arg->global_top = pt;
}

ENGINE_Term copy_to_TopHeap(Argdecl, ENGINE_Term term)
{
  int i;

  ENGINE_Term *pt = Arg->global_top;
  ENGINE_Term resul = 0xe0000000 + ((unsigned int)pt - MallocBase);
  ENGINE_Term *index = pt;


  int arity = Arity(TagToHeadfunctor(term));
  ENGINE_Term *i_term = TagToPointer(term);
  
  for (i = 0; i <= arity; i++)
    {
      HeapPush(pt, *i_term);
      i_term++;
    }

  make_ground(w, index, pt, pt); 
  return resul;
}

void insert_cont_call(Argdecl, struct EACH_CALL *callid, ENGINE_Term ContCall)
{

  make_ground(w, TagToPointer(ContCall), TagToPointer(ContCall) + 4, Arg->global_top);

  //get_free vars que sea parte de make ground
  // EEE struct untrail_list *uList = get_free_vars(NULL,ContCall);

  ENGINE_Term *pt = Arg->global_top;
  ENGINE_Term list = 0xc0000000 + ((unsigned int)pt - MallocBase);
  HeapPush(pt, ContCall);

  //HeapPush(pt, 0xc0000000 + ((unsigned int)(pt + 1) - MallocBase));
  //HeapPush(pt, (unsigned int)pt - MallocBase);
  //HeapPush(pt, 0xc0000000 + ((unsigned int)(pt + 1) - MallocBase));
  //HeapPush(pt, 0xe0000000 + ((unsigned int)(pt + 2) - MallocBase));
  ENGINE_Term free_var = (unsigned int)pt - MallocBase;
  HeapPush(pt, free_var);
  //HeapPush(pt, MakeFunctorFix);
  //HeapPush(pt, (ENGINE_Term)uList);
  //HeapPush(pt, MakeFunctorFix);

  if (callid->last_cont == callid->conts) callid->conts = list;
  else CTagToPointer(callid->last_cont) = list;

  callid->last_cont = free_var;

  Arg->global_top = pt;
  Arg->global_uncond = (tagged_t) pt;

  freeze_heap(Arg->misc->goal_desc_ptr->worker_registers->node,pt);
}

void insert_answer(Argdecl, struct EACH_CALL *callid, ENGINE_Term Answer)
{
  
  ENGINE_Term answer = copy_to_TopHeap(Arg, Answer);

  //struct untrail_list *uList = get_free_vars(NULL,answer);

  ENGINE_Term *pt = Arg->global_top;
  ENGINE_Term list = 0xc0000000 + ((unsigned int)pt - MallocBase);
  HeapPush(pt, answer);

  //HeapPush(pt, 0xc0000000 + ((unsigned int)(pt + 1) - MallocBase));
  //ENGINE_Term mark = (unsigned int)pt - MallocBase;
  //HeapPush(pt, mark);
  //ENGINE_Unify(mark,0x80000004);
  //HeapPush(pt, 0xc0000000 + ((unsigned int)(pt + 1) - MallocBase));
  //HeapPush(pt, 0xe0000000 + ((unsigned int)(pt + 2) - MallocBase));
  ENGINE_Term free_var = (unsigned int)pt - MallocBase;
  HeapPush(pt, free_var);
  //HeapPush(pt, MakeFunctorFix);
  //HeapPush(pt, (ENGINE_Term)uList);
  //HeapPush(pt, MakeFunctorFix);

  if (callid->last_answer == callid->answers) callid->answers = list;
  else CTagToPointer(callid->last_answer) = list;

  callid->last_answer = free_var;

  Arg->global_top = pt;
  Arg->global_uncond = (tagged_t) pt;

  freeze_heap(Arg->misc->goal_desc_ptr->worker_registers->node,pt);
}

/* -------------------------- */
/*            API             */     
/* -------------------------- */

void panic (char *what) 
{
    fprintf(stderr,"panic at :%s\n",what);
    exit(0);
}

inline void *mem_alloc(int size) 
{
    void *tmp;
    tmp = (void *)memory_free;

    memory_free += (size / sizeof(long));
    CHECK_MEM;
    return (tmp);
}

bool_t test_complete_c(Arg)
     Argdecl;
{
  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG1);
  complete(callid,callid);
  return TRUE;
}

bool_t clean_tables_c(Arg)
     Argdecl;
{
  if (sharedGtop == NULL) return TRUE;
  memory_free = &memory[0];
  Arg->misc->goal_desc_ptr->worker_registers->node->global_top = ini;
  Arg->global_top = ini;
  sharedGtop = NULL;
  node_top = NULL;
  return TRUE;
}

bool_t tabled_call_c(Arg)
     Argdecl;
{

  if (node_top == NULL) node_top = open_trie();

  ENGINE_Term Call;
  DEREF(Call, ENGINE_ARG1);

  struct EACH_CALL *callid = handlecall(Call); 
  ENGINE_Term Sid;
  Sid = MakeInteger(Arg, (unsigned int)callid);
  //DEREF(ENGINE_ARG2,ENGINE_ARG2);
  //ULTIMA
  ENGINE_Unify(ENGINE_ARG2,Sid);
  //CTagToPointer(ENGINE_ARG2) = Sid;
      
  if (sharedGtop == NULL) 
    {
      ini = Arg->misc->goal_desc_ptr->worker_registers->node->global_top;
      sharedGtop = (ENGINE_Term*) mem_alloc (sizeof(ENGINE_Term));
      *sharedGtop = (ENGINE_Term) Arg->misc->goal_desc_ptr->worker_registers->node->global_top;
      Arg->misc->goal_desc_ptr->worker_registers->node->global_top = sharedGtop;
    }
  
  if (callid->comp == READY) 
    {
      callid->comp = EVALUATING;
      ENGINE_Term *pt = Arg->global_top;
      HeapPush(pt, SetArity(MakeString(ENGINE_AtomName(ENGINE_ARG3)), 2));
      HeapPush(pt, Call);
      HeapPush(pt, Sid);
      Arg->global_top = pt;
      //DEREF(ENGINE_ARG5, ENGINE_ARG5);
      ENGINE_Unify(ENGINE_ARG5, Tag(STR, HeapOffset(pt, -3)));
      //CTagToPointer(ENGINE_ARG5) = Tag(STR, HeapOffset(pt, -3));
    }
  
  callid->name_cont = ENGINE_AtomName(ENGINE_ARG4);

  return TRUE;
}

bool_t is_lider_c(Arg)
     Argdecl;
{
  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG1);

  return (callid->comp == COMPLETE);
}

bool_t read_answers_c(Arg)
     Argdecl;
{
  push_choicept(Arg,address_nd_read_answers_c);

  ENGINE_Term Sid;
  DEREF(Sid,ENGINE_ARG1);

  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG2);

  if (callid->comp==EVALUATING)
    {
      struct EACH_CALL *parentcallid = (struct EACH_CALL *)ENGINE_IntOfTerm(Sid);
      SETMIN(parentcallid->plink, callid->plink);  
    }

  if (callid->ans_cnt != 0)
    {
      //push_choicept(Arg,address_nd_read_answers_c);
      ENGINE_Term *pt = Arg->global_top;
      HeapPush(pt, SetArity(MakeString(callid->name_cont), 3));
      HeapPush(pt, (unsigned int)pt - MallocBase);
      HeapPush(pt, Sid);
      DEREF(Sid, ENGINE_ARG3);
      HeapPush(pt, Sid);

      Arg->global_top = pt;
      Arg->global_uncond = (tagged_t) pt;
      node_t *n = Arg->misc->goal_desc_ptr->worker_registers->node;
      n->global_top = pt;

      //DEREF(Sid,ENGINE_ARG4);
      ENGINE_Unify(ENGINE_ARG4, Tag(STR, HeapOffset(pt, -4)));
      //CTagToPointer(Sid) = Tag(STR, HeapOffset(pt, -4));
      //DEREF(Sid,Sid);
      DEREF(Sid,ENGINE_ARG4);
      n->term[4] = Sid;
      ENGINE_Unify(*(TagToPointer(Sid) + 1), ENGINE_HeadOfTerm(callid->answers));
      //*(TagToPointer(Sid) + 1) = ENGINE_HeadOfTerm(callid->answers);
      //ENGINE_Term tail = ENGINE_TailOfTerm();
      /*Sid = ENGINE_HeadOfTerm(tail);
      tail = ENGINE_TailOfTerm(tail);

      if (IsFreeVar(Sid)) ENGINE_Unify(Sid,0x80000004);
      else 
	{
	  printf("\nToy con historias de binds L372 - untrailList = %p\n",ENGINE_IntOfTerm(ENGINE_HeadOfTerm(tail)));
	  n->term[4] = MakeSmall(((unsigned int)get_binds(ENGINE_HeadOfTerm(tail)) - MallocBase) >> 2);
	}
      */
      n->term[5] = callid->answers;
      return TRUE;
    }
  
  if(callid->comp==EVALUATING) 
    {
      ENGINE_Term *pt = Arg->global_top;
      HeapPush(pt, SetArity(MakeString(callid->name_cont), 3));
      HeapPush(pt, (unsigned int)pt - MallocBase);
      HeapPush(pt, Sid);
      DEREF(Sid, ENGINE_ARG3);
      HeapPush(pt, Sid);
      Arg->global_top = pt;
      insert_cont_call(Arg, callid, Tag(STR, HeapOffset(pt, -4)));
    }

  pop_choicept(Arg);
  return FALSE;
}

bool_t nd_read_answers_c(Arg)
     Argdecl;
{
  ENGINE_Term F;
  ENGINE_Unify(ENGINE_ARG4,ENGINE_ARG5);
  DEREF(F,ENGINE_ARG4);

  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG2);

  node_t *n = Arg->misc->goal_desc_ptr->worker_registers->node; 
  /*if (TagIsSmall(n->term[4]))
    {
      printf("\nToy con historias de binds L703 - get_binds = %p\n",(ENGINE_IntOfTerm(ENGINE_ARG7) << 2) + MallocBase);
      put_binds(ENGINE_HeadOfTerm(0xc0000000 | (ENGINE_IntOfTerm(ENGINE_ARG6) << 2)),
		(struct untrail_list*)((ENGINE_IntOfTerm(ENGINE_ARG5) << 2) + MallocBase));
      n->term[5] = 0xa0000000; //no a small integer 
    }

    ENGINE_Term tail = ENGINE_TailOfTerm(0xc0000000 | (ENGINE_IntOfTerm(ENGINE_ARG6) << 2));*/

  ENGINE_Term tail = ENGINE_TailOfTerm(ENGINE_ARG6);

  //printf("\nCompara %p con %p en %p\n",tail, callid->last_answer,callid); fflush(stdout);
  if(tail == callid->last_answer)
    {
      if (callid->comp == EVALUATING) insert_cont_call(Arg, callid, F);
      pop_choicept(Arg);
      return FALSE;
    }

  //printf("\n F = %p Leo respuesta %p ND de %p\n",F,ENGINE_HeadOfTerm(tail),tail);

  //ENGINE_Unify(*(TagToPointer(F) + 1), ENGINE_HeadOfTerm(tail));
  *(TagToPointer(F) + 1) = ENGINE_HeadOfTerm(tail);

  //tail = ENGINE_TailOfTerm(tail);
  /*F = ENGINE_HeadOfTerm(tail);
  tail = ENGINE_TailOfTerm(tail);

  if (IsFreeVar(F)) ENGINE_Unify(F,0x80000004);
  else 
    {
      //printf("\nToy con historias de binds L465 - untrailList = %p\n",ENGINE_IntOfTerm(untrailList));
      n->term[4] = MakeSmall(((unsigned int)get_binds(ENGINE_HeadOfTerm(tail)) - MallocBase) >> 2);
    }
  
  */
  n->term[5] = tail;

  return TRUE;
}

int set_diff_answer_trie(struct EACH_CALL *call, ENGINE_Term Ans)
{
  
  TrNode node;
  node=put_trie_entry(call->trie_answers,Ans,MODE_STANDARD);

  if (node->child==NULL && node != call->node_last)
    {
      if (call->node_first==NULL) call->node_first=node;

      if(call->node_last!=NULL) call->node_last->child=node;
      
      call->node_last=node;
      call->ans_cnt=1;
    
      return TRUE;
    }
  else return FALSE;
}

bool_t new_answer_c(Arg)
     Argdecl;
{
  ENGINE_Term Answer;
  DEREF(Answer, ENGINE_ARG1);

  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG2);

  if ( !set_diff_answer_trie(callid, Answer)) return FALSE;  

  insert_answer(Arg,callid,Answer);

  if (callid->conts == callid->last_cont) return FALSE;

  ENGINE_Term F = ENGINE_ARG3;
  //DEREF(F,ENGINE_ARG3);

  ENGINE_Unify(F, ENGINE_HeadOfTerm(callid->conts));
  //CTagToPointer(F) = ENGINE_HeadOfTerm(callid->conts);
  push_choicept(Arg,address_nd_new_answer_c);

  DEREF(F,F);
  ENGINE_Unify(*(TagToPointer(F) + 1), Answer);
  //*(TagToPointer(F) + 1) = Answer;

  //Answer = ENGINE_TailOfTerm(callid->conts);
  node_t *n = Arg->misc->goal_desc_ptr->worker_registers->node; 
  /*F = ENGINE_HeadOfTerm(Answer);
  Answer = ENGINE_TailOfTerm(Answer);
  

  if (IsFreeVar(F)) ENGINE_Unify(F,0x80000004);
  else 
    {
      //printf("\nToy con historias de binds L531\n"); fflush(stdout);
      n->term[3] = MakeSmall(((unsigned int)get_binds(ENGINE_HeadOfTerm(Answer)) - MallocBase) >> 2);
    }
  
  */
  n->term[4] = callid->conts;
 
  return TRUE;
}

bool_t nd_new_answer_c(Arg)
     Argdecl;
{
  node_t *n = Arg->misc->goal_desc_ptr->worker_registers->node; 
  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG2);

  /*
  if (TagIsSmall(n->term[3]))
    {
      //printf("\nHistorias de binds\n");
      put_binds(ENGINE_HeadOfTerm(0xc0000000 | (ENGINE_IntOfTerm(ENGINE_ARG5) << 2)),
		(struct untrail_list*)((ENGINE_IntOfTerm(ENGINE_ARG4) << 2) + MallocBase));
      n->term[3] = 0xa0000000; //no a small integer 
    }

    ENGINE_Term tail = ENGINE_TailOfTerm(0xc0000000 | (ENGINE_IntOfTerm(ENGINE_ARG5) << 2));*/

  ENGINE_Term tail = ENGINE_TailOfTerm(ENGINE_ARG5);

  if (tail == callid->last_cont) 
    {
      pop_choicept(Arg);
      return FALSE;
    }
  
  ENGINE_Term F = ENGINE_ARG3;
  //DEREF(F,ENGINE_ARG3);
  ENGINE_Unify(F, ENGINE_HeadOfTerm(tail));
  //CTagToPointer(F) = ENGINE_HeadOfTerm(tail);
  DEREF(F,F);
  //DEREF(ENGINE_ARG1,ENGINE_ARG1);
  ENGINE_Unify(*(TagToPointer(F) + 1), ENGINE_ARG1);
  //*(TagToPointer(F) + 1) = ENGINE_ARG1;

  //tail = ENGINE_TailOfTerm(tail);
  /*F = ENGINE_HeadOfTerm(tail);
  tail = ENGINE_TailOfTerm(tail);
  
  if (IsFreeVar(F)) ENGINE_Unify(F,0x80000004);
  else 
    {
      //printf("\nToy con historias de binds L582 con callid %p y conts %p\n",callid,callid->conts); fflush(stdout);
      //printf("\nArgumento 1 %p\n",*(TagToPointer(ENGINE_ARG1) + 1)); fflush(stdout);
      n->term[3] = MakeSmall(((unsigned int)get_binds(ENGINE_HeadOfTerm(tail)) - MallocBase) >> 2);
    }
  
  */

  n->term[4] = tail;

  return TRUE;
}

bool_t consume_answer_c(Arg)
     Argdecl;
{
  struct EACH_CALL *callid=(struct EACH_CALL *) (ENGINE_IntOfTerm(ENGINE_ARG2));
  
  if (callid->comp != COMPLETE) return FALSE; //lanzar continuaciones

  if(callid->node_first==NULL) return FALSE;

  TrNode node = callid->node_first;

  if (node==NULL) return FALSE;

  push_choicept(Arg,address_nd_consume_answer_c);

  if(ENGINE_Unify(ENGINE_ARG1,get_trie_entry(Arg,callid->node_first,MODE_STANDARD)))
    {
      Arg->misc->goal_desc_ptr->worker_registers->node->term[2] = (ENGINE_Term)node;
	/*
	  ENGINE_Term * term = (ENGINE_Term*) &(Arg->misc->goal_desc_ptr->worker_registers->node->term[1]);
      *term = (ENGINE_Term) ((int)id_solution - MallocBase);
      *term = (ENGINE_Term) ((int)*term << 2);
      *term = (ENGINE_Term) ((int)*term | 0x80000000);
      */
      return TRUE;
    }

  //pop_choicept(Arg);
  return FALSE;
}

bool_t nd_consume_answer_c(Arg)
     Argdecl;
{
  TrNode node;
 
  /*ENGINE_Term *id_solution = (ENGINE_Term*) (ENGINE_ARG2 & 0x0FFFFFFF);
  id_solution = (ENGINE_Term*) ((int)id_solution >> 2);
  id_solution = (ENGINE_Term*) ((int)id_solution + MallocBase);
  */
  
  node = (TrNode) ENGINE_ARG3;

  node=node->child;

  if(node==NULL) 
    {
      pop_choicept(Arg);
      return FALSE;
    }

  Arg->misc->goal_desc_ptr->worker_registers->node->term[2] = (ENGINE_Term)node;

  if(ENGINE_Unify(ENGINE_ARG1,get_trie_entry(Arg,node,MODE_STANDARD))) return TRUE;
  
  //pop_choicept(Arg);
  return FALSE;  

}

bool_t initial_c(Arg)
     Argdecl;
{
  address_nd_consume_answer_c = def_retry_c(nd_consume_answer_c,3);
  address_nd_read_answers_c = def_retry_c(nd_read_answers_c,6);
  address_nd_new_answer_c = def_retry_c(nd_new_answer_c,5);
  init_tries_module();
  return TRUE;
}
