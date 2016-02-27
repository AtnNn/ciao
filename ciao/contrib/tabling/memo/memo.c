/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include "memo.h"
#include "tries.c"

/* -------------------------- */
/*     Inline Procedures      */
/* -------------------------- */

tagged_t *ini = NULL;
//ENGINE_Term *paux = NULL;
ENGINE_Term array[TERM_STACK_SIZE];

ENGINE_Term my_make_var(goal_descriptor_t *state)
{
  ciao_ensure_heap(state, 1);
  ENGINE_Term resul = TagHVA(GTOP);
  HeapPush(GTOP, resul);
  return resul;
}			      

ENGINE_Term my_make_integer(goal_descriptor_t *state, int i) 
{
  ciao_ensure_heap(state, 4);  //change to in my_heap
  return MakeInteger(REGISTERS, i);
}

ENGINE_Term my_make_float(goal_descriptor_t *state, double f) 
{
  ciao_ensure_heap(state, 4);
  return MakeFloat(REGISTERS, f);
}

ENGINE_Term my_make_list(goal_descriptor_t *state, ENGINE_Term head, ENGINE_Term tail) 
{
  ENGINE_Term list;
  ciao_ensure_heap(state, 3);
  worker_t * w = REGISTERS;
  MakeLST(list, head, tail);
  return list;
}

ENGINE_Term my_make_functor(goal_descriptor_t *state, char* name, int arity, ENGINE_Term *args) 
{
  worker_t * w = REGISTERS;
  if (arity == 0) return MakeString((char *)name);
  else if (strcmp(name, ".") == 0 && arity == 2) 
    {
      ENGINE_Term list;
      ciao_ensure_heap(state, 3);
      MakeLST(list, args[0], args[1]);
      return list;
    } 
  else 
    {
      int i;
      ciao_ensure_heap(state, 2 + arity);
      HeapPush(P_GTOP, SetArity(MakeString((char *)name), arity));
      for (i = 0; i < arity; i++) HeapPush(P_GTOP, args[i]);
      return Tag(STR, HeapOffset(P_GTOP, -(arity+1)));
    }
}

struct EACH_CALL *handlecall(struct memo_register *memo, ENGINE_Term Call) 
{	     
  struct EACH_CALL *callid;
  TrNode node = put_trie_entry(memo,(TrNode)memo->node_top, Call);

  if (node->child == NULL) 
    {
      node->child = (trie_node_t *)mem_alloc(memo, sizeof(struct EACH_CALL));
      callid = (struct EACH_CALL *)(node->child);
      
      callid->node_list = NULL; //No answers

      callid->comp = READY;
      //memzero
      callid->trie_answers = open_trie(memo);
//      callid->node_first=NULL;
//      callid->node_last=NULL;
//      callid->previous=NULL;
      return callid;
    }
  else return (struct EACH_CALL *)(node->child);
}

void panic (char *what) 
{
    fprintf(stderr,"panic at :%s\n",what);
    exit(0);
}

void *mem_alloc(struct memo_register *memo, int size) 
{
    void *tmp;
    int i;
    tmp = (void *)memo->memory_free;

    memo->memory_free += (size / sizeof(long));
    CHECK_MEM;
    for (i = 0; i < (size / sizeof(long)); i++) ((long*)tmp)[i] = 0;
    return (tmp);
}

bool_t clean_tables_c(Arg)
     Argdecl;
{
  if (AMEMO != NULL) 
    {
      AMEMO->memory_free = &(AMEMO->memory[0]);
      AMEMO->memory_end =  &(AMEMO->memory[0]) + MEMSIZE*1024;
      AMEMO->node_top = NULL;
    }
  return TRUE;
}

bool_t set_complete_c(Arg)
     Argdecl;
{
  struct EACH_CALL *call = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG1);
  call->comp = COMPLETE;

  return FALSE;
}

bool_t memo_call_c(Arg)
     Argdecl;
{

  if (Arg->misc->memo == NULL) 
    {
      Arg->misc->memo = (void*) malloc (sizeof(struct memo_register));
      AMEMO->memory_free = &(AMEMO->memory[0]);
      AMEMO->memory_end = &(AMEMO->memory[0]) + MEMSIZE*1024;
      AMEMO->node_top = (void *)open_trie(AMEMO);
    }

  if (AMEMO->node_top == NULL) AMEMO->node_top = (void *)open_trie(AMEMO);

  struct EACH_CALL *callid = handlecall(AMEMO,ENGINE_ARG1); 

  ENGINE_Unify(ENGINE_ARG2,MakeInteger(Arg, (unsigned int)callid));
  DEREF(ENGINE_ARG2,ENGINE_ARG2);

  if (callid->comp == COMPLETE) 
    {
      ENGINE_Term *pt = Arg->global_top;
      HeapPush(pt, SetArity(MakeString("true"), 0));
      Arg->global_top = pt;
      ENGINE_Unify(ENGINE_ARG3, Tag(STR, HeapOffset(pt, -1)));
    }
  else if (callid->comp == EVALUATING) 
    {
      printf("\nThere is an infinite fail\n");
      exit(0);
    }
  
  return TRUE;
}

bool_t new_answer_c(Arg)
     Argdecl;
{
  struct EACH_CALL *call = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG2);

  TrNode node = put_trie_entry(AMEMO,call->trie_answers,ENGINE_ARG1);

  if (call->node_list==NULL) 
    {
      call->node_list = (node_list_t*) malloc (sizeof(node_list_t));
      call->node_list->node = node;
      call->node_list->node_sig = NULL;
      call->node_last = call->node_list;
    }
  else
    {
      call->node_last->node_sig = (node_list_t*) malloc (sizeof(node_list_t));
      call->node_last->node_sig->node = node;
      call->node_last->node_sig->node_sig = NULL;
      call->node_last = call->node_last->node_sig;
    }

  return TRUE;
}

bool_t consume_answer_c(Arg)
     Argdecl;
{
  struct EACH_CALL *callid=(struct EACH_CALL *) (ENGINE_IntOfTerm(ENGINE_ARG2));
  
  if (callid->comp != COMPLETE) return FALSE; //lanzar continuaciones

  if (callid->node_list == NULL) return FALSE;

  TrNode node = callid->node_list->node;
  int num_ans = 1;
 
  push_choicept(Arg,address_nd_consume_answer_c);

  Arg->node->term[2] = MakeInteger(Arg, (int)callid->node_list);

  if(ENGINE_Unify(ENGINE_ARG1,get_trie_entry(Arg,node))) return TRUE;

  return FALSE;
}

bool_t nd_consume_answer_c(Arg)
     Argdecl;
{
  node_list_t* node_list =  (node_list_t*) ENGINE_IntOfTerm(ENGINE_ARG3);
  struct EACH_CALL *callid=(struct EACH_CALL *) (ENGINE_IntOfTerm(ENGINE_ARG2));

  node_list = node_list->node_sig;
  if (node_list == NULL)
    {
      pop_choicept(Arg);
      return FALSE;
    }

  TrNode node = node_list->node;
  Arg->node->term[2] = MakeInteger(Arg, (int)node_list);

  if(ENGINE_Unify(ENGINE_ARG1,get_trie_entry(Arg,node))) return TRUE;
  
  return FALSE;  

}

bool_t initial_c(Arg)
     Argdecl;
{
  Arg->misc->memo = NULL;
  address_nd_consume_answer_c = def_retry_c(nd_consume_answer_c,3);
  init_tries_module();
  return TRUE;
}
