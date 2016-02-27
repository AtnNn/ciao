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
      callid->conts = ENGINE_MkAtomTerm("[]");

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

void make_ground(Argdecl, ENGINE_Term *index, ENGINE_Term *indexFin)
{
  ENGINE_Term *i_term;
  int i, arity;
  ENGINE_Term *ptIni = tabled_top;

  while(index < tabled_top)
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
	  HeapPush(index, 0xc0000000 + ((unsigned int)tabled_top - MallocBase));
	  ENGINE_Term *list = TagToPointer(t);
	  HeapPush(tabled_top, *list);
	  list++;
	  HeapPush(tabled_top, *list);
	}
      else if (TagIsSTR(t))
	{
	  if (IsNumber(t))
	    {
	     if (TagIsSmall(t)) index++; 
	     else
	       {
		 HeapPush(index, 0xe0000000 + ((unsigned int)tabled_top - MallocBase));
		 HeapPush(tabled_top, 0xb0000008);
		 HeapPush(tabled_top, *(TagToPointer(t) + 1));
		 HeapPush(tabled_top, 0xb0000008);
	       }
	    }
	  else
	    {
	      HeapPush(index, 0xe0000000 + ((unsigned int)tabled_top - MallocBase));
	      i_term = TagToPointer(t);
	      arity = Arity(TagToHeadfunctor(t));

	      for (i = 0; i <= arity; i++)
		{
		  HeapPush(tabled_top, *i_term);
		  i_term++;
		}
	    }
	}
      else index++;

      if (index == indexFin) index = ptIni;
    }
}

ENGINE_Term copy_to_UnderHeap(Argdecl, ENGINE_Term term)
{
  int i;

  ENGINE_Term resul = 0xe0000000 + ((unsigned int)tabled_top - MallocBase);
  ENGINE_Term *index = tabled_top;


  int arity = Arity(TagToHeadfunctor(term));
  ENGINE_Term *i_term = TagToPointer(term);
  
  for (i = 0; i <= arity; i++)
    {
      HeapPush(tabled_top, *i_term);
      i_term++;
    }

  make_ground(Arg, index, tabled_top); 
  return resul;
}

void insert_cont_call(Argdecl, struct EACH_CALL *callid, ENGINE_Term ContCall)
{

  make_ground(Arg, TagToPointer(ContCall), TagToPointer(ContCall) + 4);

  HeapPush(tabled_top, ContCall);
  HeapPush(tabled_top, callid->conts);

  callid->conts = 0xc0000000 + (tagged_t)(tabled_top - 2) - MallocBase;
}

void insert_answer(Argdecl, struct EACH_CALL *callid, ENGINE_Term Answer)
{
  
  ENGINE_Term answer = copy_to_UnderHeap(Arg, Answer);

  ENGINE_Term list = 0xc0000000 + ((unsigned int)tabled_top - MallocBase);

  HeapPush(tabled_top, answer);
  HeapPush(tabled_top, (unsigned int)tabled_top - MallocBase);

  if (callid->last_answer == callid->answers) callid->answers = list;
  else CTagToPointer(callid->last_answer) = list;

  callid->last_answer = (unsigned int)(tabled_top - 1) - MallocBase;
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
  if (ini == NULL) return TRUE;
  memory_free = &memory[0];
  tabled_top = ini;
  node_top = NULL;
  return TRUE;
}

bool_t tabled_call_c(Arg)
     Argdecl;
{
  //if ((TOP_SF != NULL) && el predicado es true) Tab -> Prolog -> Tab

  if (node_top == NULL) node_top = open_trie();

  DEREF(ENGINE_ARG1, ENGINE_ARG1);

  struct EACH_CALL *callid = handlecall(ENGINE_ARG1); 
  ENGINE_Unify(ENGINE_ARG2,MakeInteger(Arg, (unsigned int)callid));
  DEREF(ENGINE_ARG2,ENGINE_ARG2);
      
  if (ini == NULL) ini = tabled_top;
  
  if (callid->comp == READY) 
    {
      callid->comp = EVALUATING;
      ENGINE_Term *pt = Arg->global_top;
      HeapPush(pt, SetArity(MakeString(ENGINE_AtomName(ENGINE_ARG3)), 2));
      HeapPush(pt, ENGINE_ARG1);
      HeapPush(pt, ENGINE_ARG2);
      Arg->global_top = pt;
      ENGINE_Unify(ENGINE_ARG5, Tag(STR, HeapOffset(pt, -3)));
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
  DEREF(ENGINE_ARG1,ENGINE_ARG1);

  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG2);

  if (callid->comp==EVALUATING)
    {
      struct EACH_CALL *parentcallid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG1);
      SETMIN(parentcallid->plink, callid->plink);  
    }

  if (callid->ans_cnt != 0)
    {
      HeapPush(tabled_top, SetArity(MakeString(callid->name_cont), 3));
      HeapPush(tabled_top, (unsigned int)tabled_top - MallocBase);
      HeapPush(tabled_top, ENGINE_ARG1);
      DEREF(ENGINE_ARG3, ENGINE_ARG3);
      HeapPush(tabled_top, ENGINE_ARG3);

      ENGINE_Unify(ENGINE_ARG4, Tag(STR, HeapOffset(tabled_top, -4)));
      push_choicept(Arg,address_nd_read_answers_c);

      DEREF(ENGINE_ARG4,ENGINE_ARG4);
      *(TagToPointer(ENGINE_ARG4) + 1) = ENGINE_HeadOfTerm(callid->answers);

      Arg->node->term[0] = callid->answers;
      return TRUE;
    }
  
  if(callid->comp==EVALUATING) 
    {
      HeapPush(tabled_top, SetArity(MakeString(callid->name_cont), 3));
      HeapPush(tabled_top, (unsigned int)tabled_top - MallocBase);
      HeapPush(tabled_top, ENGINE_ARG1);
      DEREF(ENGINE_ARG3, ENGINE_ARG3);
      HeapPush(tabled_top, ENGINE_ARG3);
      insert_cont_call(Arg, callid, Tag(STR, HeapOffset(tabled_top, -4)));
    }

  return FALSE;
}

bool_t nd_read_answers_c(Arg)
     Argdecl;
{
  DEREF(ENGINE_ARG4,ENGINE_ARG4);
  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG2);

  ENGINE_ARG1 = ENGINE_TailOfTerm(ENGINE_ARG1);

  if(ENGINE_ARG1 == callid->last_answer)
    {
      if (callid->comp == EVALUATING) 
	{
	  insert_cont_call(Arg, callid, ENGINE_ARG4);
	}
      pop_choicept(Arg);
      return FALSE;
    }


  *(TagToPointer(ENGINE_ARG4) + 1) = ENGINE_HeadOfTerm(ENGINE_ARG1);

  Arg->node->term[0] = ENGINE_ARG1;

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

  DEREF(ENGINE_ARG1, ENGINE_ARG1);
  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG2);

  if ( !set_diff_answer_trie(callid, ENGINE_ARG1)) return FALSE;  

  insert_answer(Arg,callid,ENGINE_ARG1);

  if (callid->conts == ENGINE_MkAtomTerm("[]")) return FALSE;

  ENGINE_Unify(ENGINE_ARG3, ENGINE_HeadOfTerm(callid->conts));
  push_choicept(Arg,address_nd_new_answer_c);

  DEREF(ENGINE_ARG3,ENGINE_ARG3);
  ENGINE_Unify(*(TagToPointer(ENGINE_ARG3) + 1), ENGINE_ARG1);
  Arg->node->term[1] = callid->conts;
 
  return TRUE;
}

bool_t nd_new_answer_c(Arg)
     Argdecl;
{
  ENGINE_ARG2 = ENGINE_TailOfTerm(ENGINE_ARG2);

  if (ENGINE_ARG2 == ENGINE_MkAtomTerm("[]")) //conprobar si es la lista vacia 
    {
      pop_choicept(Arg);
      return FALSE;
    }
  
  CTagToPointer(ENGINE_ARG3) = ENGINE_HeadOfTerm(ENGINE_ARG2);
  DEREF(ENGINE_ARG3,ENGINE_ARG3);
  ENGINE_Unify(*(TagToPointer(ENGINE_ARG3) + 1), ENGINE_ARG1);

  Arg->node->term[1] = ENGINE_ARG2;

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
      Arg->node->term[1] = (ENGINE_Term)node;
      return TRUE;
    }

  return FALSE;
}

bool_t nd_consume_answer_c(Arg)
     Argdecl;
{
  TrNode node = (TrNode) ENGINE_ARG2;

  node=node->child;

  if(node==NULL) 
    {
      pop_choicept(Arg);
      return FALSE;
    }

  Arg->node->term[1] = (ENGINE_Term)node;

  if(ENGINE_Unify(ENGINE_ARG1,get_trie_entry(Arg,node,MODE_STANDARD))) return TRUE;
  
  return FALSE;  

}

bool_t initial_c(Arg)
     Argdecl;
{
  address_nd_consume_answer_c = def_retry_c(nd_consume_answer_c,2);
  address_nd_read_answers_c = def_retry_c(nd_read_answers_c,4);
  address_nd_new_answer_c = def_retry_c(nd_new_answer_c,3);
  init_tries_module();
  return TRUE;
}
