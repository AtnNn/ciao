/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include "cont_calls.h"
#include "tries.c"

/* -------------------------- */
/*     Inline Procedures      */
/* -------------------------- */

tagged_t *ini = NULL;
ENGINE_Term *paux = NULL;
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

struct EACH_CALL *handlecall(ENGINE_Term Call) 
{	     
  struct EACH_CALL *callid;
  TrNode node = put_trie_entry(node_top, Call);

  if (node->child == NULL) 
    {
      node->child = (trie_node_t *)mem_alloc(sizeof(struct EACH_CALL));
      callid = (struct EACH_CALL *)(node->child);

      callid->answers = Tag(LST,tabled_top); //0xc0000000 + ((unsigned int)tabled_top - MallocBase);
      HeapPush(tabled_top, TagHVA(tabled_top)); //(unsigned int)tabled_top - MallocBase);
      HeapPush(tabled_top, TagHVA(tabled_top)); 
      callid->last_answer = *(tabled_top - 1);

      callid->conts = EMPTY_LIST;
      callid->trie_cont = open_trie();
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
  return TRUE;
}

void make_ground(Argdecl, ENGINE_Term *index, ENGINE_Term *indexFin)
{
  ENGINE_Term *i_term;
  int i, arity;
  ENGINE_Term *ptIni = tabled_top;
  ENGINE_Term *indexIni = index;
  ENGINE_Term *pt = tabled_top;
  int iarray = 0;

  while(index < tabled_top)
    {
      ENGINE_Term t = *index;
      if (t == INTEGER_MARK) index = index + 3;
      else if (t == FLOAT_MARK) index = index + 4;
      else if (IsVar(t) || IsTrieVar(t))
	{
	  DEREF(t,t);
	  if (IsTrieVar(t))  
	    {
	      HeapPush(index, array[TrieVarIndex(t)]);
	    }
	  else if (IsVar(t)) 
	    {
	      if (iarray + 2 >= TERM_STACK_SIZE)
		fprintf(stderr, "\ncont_call module: TERM_STACK full");
	      
	      array[iarray++] = (unsigned int)index - MallocBase;
	      HeapPush(index, (unsigned int)index - MallocBase);
	      array[iarray] = t;
	      CTagToPointer(t) = ENGINE_VarTrie | ((iarray-1) << 1);
	      iarray++;
	    }
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
		 if (!LargeIsFloat(t)) //large integer
		   {
		     HeapPush(index, 0xe0000000 + ((unsigned int)tabled_top - MallocBase));
		     HeapPush(tabled_top, INTEGER_MARK);
		     HeapPush(tabled_top, *(TagToPointer(t) + 1));
		     HeapPush(tabled_top, INTEGER_MARK);
		   }
		 else //float
		   {
		     HeapPush(index, 0xe0000000 + ((unsigned int)tabled_top - MallocBase));
		     HeapPush(tabled_top, FLOAT_MARK);
		     HeapPush(tabled_top, *(TagToPointer(t) + 1));
		     HeapPush(tabled_top, *(TagToPointer(t) + 2));
		     HeapPush(tabled_top, FLOAT_MARK);
		   }
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

  //Reinstall original values of variables.
  int j;
  for(j = 1; j < iarray; j = j + 2) 
    {
      CTagToPointer(array[j]) = array[j];
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

void insert_cont_call(Argdecl, struct EACH_CALL *callid, ENGINE_Term ContCall, int flag)
{

  if (flag) make_ground(Arg, TagToPointer(ContCall), TagToPointer(ContCall) + 4);
  else make_ground(Arg, TagToPointer(ContCall), TagToPointer(ContCall) + 5);

  ENGINE_Term list = 0xc0000000 + ((unsigned int)tabled_top - MallocBase);
  HeapPush(tabled_top, ContCall);

  HeapPush(tabled_top, 0xc0000000 + ((unsigned int)(tabled_top + 1) - MallocBase));
  HeapPush(tabled_top, callid->answers);
  HeapPush(tabled_top, callid->conts);

  callid->conts = list;
}

void insert_answer(Argdecl, struct EACH_CALL *callid, ENGINE_Term Answer)
{
  
  ENGINE_Term answer = copy_to_UnderHeap(Arg, Answer);

  ENGINE_Term list = 0xc0000000 + ((unsigned int)tabled_top - MallocBase);
  HeapPush(tabled_top, answer);

  ENGINE_Term free_var = (unsigned int)tabled_top - MallocBase;
  HeapPush(tabled_top, free_var);
  CTagToPointer(callid->last_answer) = list;
  callid->last_answer = free_var;
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
  if (ini == NULL) ini = tabled_top;
  
  if (node_top == NULL) node_top = open_trie();

  struct EACH_CALL *callid = handlecall(ENGINE_ARG1); 
  ENGINE_Unify(ENGINE_ARG2,MakeInteger(Arg, (unsigned int)callid));
  DEREF(ENGINE_ARG2,ENGINE_ARG2);
  ENGINE_Term t = ENGINE_ArgOfTerm(2,ENGINE_ARG1);
//  printf("\n%p\n",t); fflush(stdout);
//  printf("\nCTAG = %p\n",CTagToPointer(t)); fflush(stdout);
//  printf("\nCTAG2 = %p\n",CTagToPointer(CTagToPointer(t))); fflush(stdout);
//  printf("\nCTAG3 = %p\n",CTagToPointer(CTagToPointer(CTagToPointer(t)))); fflush(stdout);
//  printf("\nCTAGGoal = %p\n",CTagToGoal(CTagToPointer(t))); fflush(stdout);
//  printf("\n*CTAGGoal = %p\n",CTagToPointer(CTagToGoal(CTagToPointer(t)))); fflush(stdout);
//  printf("\n*CTAGGoal1 = %p\n",*(TagToPointer(CTagToGoal(CTagToPointer(t)))+1)); fflush(stdout);
//  printf("\n*CTAGGoal2 = %p\n",*(TagToPointer(CTagToGoal(CTagToPointer(t)))+2)); fflush(stdout);
//  printf("\n*CTAGGoal3 = %p\n",*(TagToPointer(CTagToGoal(CTagToPointer(t)))+3)); fflush(stdout);
//  printf("\n*CTAGGoal4 = %p\n",*(TagToPointer(CTagToGoal(CTagToPointer(t)))+4)); fflush(stdout);

  if (callid->comp == READY) 
    {
      callid->comp = EVALUATING;
      ENGINE_Term *pt = Arg->global_top;
      HeapPush(pt, SetArity(MakeString(ENGINE_AtomName(ENGINE_ARG3)), 2));
      HeapPush(pt, ENGINE_ARG1);
      HeapPush(pt, ENGINE_ARG2);
      Arg->global_top = pt;
      ENGINE_Unify(ENGINE_ARG4, Tag(STR, HeapOffset(pt, -3)));
    }
  else
    {
      ENGINE_Term *pt = Arg->global_top;
      HeapPush(pt, SetArity(MakeString("true"), 0));
      Arg->global_top = pt;
      ENGINE_Unify(ENGINE_ARG4, Tag(STR, HeapOffset(pt, -1)));
    }
  
  return TRUE;
}

bool_t resume_ccalls_c(Arg)
     Argdecl;
{
  struct EACH_CALL *sf = NULL;
  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG1);
  do
    {
      if (sf == NULL) sf = TOP_SF;
      else sf = sf->previous;

      if (sf->conts == ENGINE_MkAtomTerm("[]")) continue;

      ENGINE_Term Answers = ENGINE_TailOfTerm(ENGINE_HeadOfTerm(ENGINE_TailOfTerm(sf->conts)));
      if (ENGINE_HeadOfTerm(Answers) != sf->last_answer)
	{
	  ENGINE_Term F = ENGINE_HeadOfTerm(sf->conts);
	  *(TagToPointer(F) + 1) = ENGINE_HeadOfTerm(Answers);
	  DEREF(ENGINE_ARG2,ENGINE_ARG2);
	  push_choicept(Arg,address_nd_resume_ccalls_c);
	  ENGINE_Unify(ENGINE_ARG2, F);

	  Arg->node->term[1] = ENGINE_ARG2; 
	  Arg->node->term[2] = (ENGINE_Term)sf; //Meterlo para que vaya recoleccion basura
	  Arg->node->term[3] = sf->conts;
	  Arg->node->term[4] = Answers;
	  return TRUE;
	}
    }
  while(sf != callid);

  complete(callid,callid);
  ENGINE_Term *pt = Arg->global_top;
  HeapPush(pt, SetArity(MakeString("true"), 0));
  Arg->global_top = pt;
  ENGINE_Unify(Tag(STR, HeapOffset(pt, -1)),ENGINE_ARG2);

  return TRUE;
}

bool_t nd_resume_ccalls_c(Arg)
     Argdecl;
{
  struct EACH_CALL *sf = (struct EACH_CALL *)ENGINE_ARG3;
  ENGINE_Term F;
  ENGINE_Term Answers = ENGINE_TailOfTerm(ENGINE_ARG5);

  if (Answers != sf->last_answer)
    {      
      F = ENGINE_HeadOfTerm(ENGINE_ARG4);
      *(TagToPointer(F) + 1) = ENGINE_HeadOfTerm(Answers);
      ENGINE_Unify(ENGINE_ARG2, F);
      Arg->node->term[4] = Answers;
      return TRUE;
    } 

  ENGINE_HeadOfTerm(ENGINE_TailOfTerm(ENGINE_ARG4)) = ENGINE_ARG5;
  F = ENGINE_TailOfTerm(ENGINE_TailOfTerm(ENGINE_ARG4));

  if (F != ENGINE_MkAtomTerm("[]"))
    {
      Answers = ENGINE_TailOfTerm(ENGINE_HeadOfTerm(ENGINE_TailOfTerm(F)));
      if (ENGINE_HeadOfTerm(Answers) != sf->last_answer)
	{
	  Arg->node->term[3] = F;
	  Arg->node->term[4] = Answers;
	  F = ENGINE_HeadOfTerm(F);
	  *(TagToPointer(F) + 1) = ENGINE_HeadOfTerm(Answers);
	  ENGINE_Unify(ENGINE_ARG2, F);
	  return TRUE;
	}
    }

  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG1);
  while(sf != callid)
    {
      sf = sf->previous;    
      if (sf->conts == ENGINE_MkAtomTerm("[]")) continue;

      Answers = ENGINE_TailOfTerm(ENGINE_HeadOfTerm(ENGINE_TailOfTerm(sf->conts)));
      if (ENGINE_HeadOfTerm(Answers) != sf->last_answer)
	{
	  F = ENGINE_HeadOfTerm(sf->conts);
	  *(TagToPointer(F) + 1) = ENGINE_HeadOfTerm(Answers);
	  Arg->node->term[2] = (ENGINE_Term)sf; //Meterlo para que vaya recoleccion basura
	  Arg->node->term[3] = sf->conts;
	  Arg->node->term[4] = Answers;
	  ENGINE_Unify(ENGINE_ARG2, F);
	  return TRUE;
	}
   }

 sf = NULL;
 do
   {
     if (sf == NULL) sf = TOP_SF;
     else sf = sf->previous;

     if (sf->conts == ENGINE_MkAtomTerm("[]")) continue;

     ENGINE_Term Answers = ENGINE_TailOfTerm(ENGINE_HeadOfTerm(ENGINE_TailOfTerm(sf->conts)));
     if (ENGINE_HeadOfTerm(Answers) != sf->last_answer)
	{
	  F = ENGINE_HeadOfTerm(sf->conts);
	  *(TagToPointer(F) + 1) = ENGINE_HeadOfTerm(Answers);
	  ENGINE_Unify(ENGINE_ARG2, F);

	  Arg->node->term[2] = (ENGINE_Term)sf; //Meterlo para que vaya recoleccion basura
	  Arg->node->term[3] = sf->conts;
	  Arg->node->term[4] = Answers;
	  return TRUE;
	}
    }
  while(sf != callid);

  complete(callid,callid);
  CTagToPointer(ENGINE_ARG2) = ENGINE_ARG2;
  ENGINE_Term *pt = Arg->global_top;
  HeapPush(pt, SetArity(MakeString("true"), 0));
  Arg->global_top = pt;
  ENGINE_Unify(Tag(STR, HeapOffset(pt, -1)),ENGINE_ARG2);

  pop_choicept(Arg);

  return TRUE;
}

bool_t new_ccall_c(Arg)
     Argdecl;
{
  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG2);
  int flag;

  DEREF(ENGINE_ARG5,ENGINE_ARG5);
  flag = (CTagToPointer(ENGINE_ARG5) == ENGINE_ARG5) || ENGINE_IsPairTerm(ENGINE_ARG5);

  if (callid->comp==EVALUATING)
    {
      struct EACH_CALL *parentcallid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG1);
      SETMIN(parentcallid->plink, callid->plink);  

      if (flag) HeapPush(tabled_top, SetArity(MakeString(ENGINE_AtomName(ENGINE_ARG4)), 3));
      else HeapPush(tabled_top, SetArity(MakeString(ENGINE_AtomName(ENGINE_ARG4)), 4));
      HeapPush(tabled_top, (unsigned int)tabled_top - MallocBase);
      HeapPush(tabled_top, ENGINE_ARG1);
      HeapPush(tabled_top, ENGINE_ARG3);
      if (!flag) HeapPush(tabled_top, ENGINE_ARG5);

      //printf("\nEs repetida?\n");
      //TrNode node;
      //if (flag) node = put_trie_entry(callid->trie_cont,Tag(STR, HeapOffset(tabled_top, -4)));
      //else node = put_trie_entry(callid->trie_cont,Tag(STR, HeapOffset(tabled_top, -5)));
      //if (node->child != NULL) return FALSE;
      //node->child = (TrNode)1;
      
      if (flag) insert_cont_call(Arg, callid, Tag(STR, HeapOffset(tabled_top, -4)),flag);
      else insert_cont_call(Arg, callid, Tag(STR, HeapOffset(tabled_top, -5)),flag);
      return FALSE;
    }

  //Return continuation consuming answers (resh copies) like consume_answer primitive
  //Unificar la respuesta, igual!!! 

  TrNode node = callid->node_first;
  if (node == NULL) return FALSE;

  ENGINE_Term Answer = get_trie_entry(Arg,node);
  ENGINE_Term *pt = Arg->global_top;
  if (flag) HeapPush(pt, SetArity(MakeString(ENGINE_AtomName(ENGINE_ARG4)), 3));
  else HeapPush(pt, SetArity(MakeString(ENGINE_AtomName(ENGINE_ARG4)), 4));
  HeapPush(pt, Answer);
  HeapPush(pt, ENGINE_ARG1);
  HeapPush(pt, ENGINE_ARG3);
  if (!flag) HeapPush(pt, ENGINE_ARG5);
  Arg->global_top = pt;

  if (flag) ENGINE_Unify(ENGINE_ARG6, Tag(STR, HeapOffset(pt, -4)));
  else ENGINE_Unify(ENGINE_ARG6, Tag(STR, HeapOffset(pt, -5)));
  push_choicept(Arg,address_nd_new_ccall_c);
  DEREF(ENGINE_ARG6,ENGINE_ARG6);
  Arg->node->term[3] = (ENGINE_Term) node;
  Arg->node->term[5] = ENGINE_ARG6;
  return TRUE;

//  ENGINE_Term Answers = ENGINE_TailOfTerm(callid->answers);
//  if (Answers == callid->last_answer) return FALSE;

//  ENGINE_Term *pt = Arg->global_top;
//  if (flag) HeapPush(pt, SetArity(MakeString(ENGINE_AtomName(ENGINE_ARG4)), 3));
//  else HeapPush(pt, SetArity(MakeString(ENGINE_AtomName(ENGINE_ARG4)), 4));
//  HeapPush(pt, ENGINE_HeadOfTerm(Answers));
//  HeapPush(pt, ENGINE_ARG1);
//  HeapPush(pt, ENGINE_ARG3);
//  if (!flag) HeapPush(pt, ENGINE_ARG5);
//  Arg->global_top = pt;
//
//  if (flag) ENGINE_Unify(ENGINE_ARG6, Tag(STR, HeapOffset(pt, -4)));
//  else ENGINE_Unify(ENGINE_ARG6, Tag(STR, HeapOffset(pt, -5)));
//  push_choicept(Arg,address_nd_new_ccall_c);
//  DEREF(ENGINE_ARG6,ENGINE_ARG6);
//  Arg->node->term[3] = Answers;
//  Arg->node->term[5] = ENGINE_ARG6;
//  return TRUE;
}
bool_t nd_new_ccall_c(Arg)
     Argdecl;
{
  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG2);
  TrNode node = (TrNode) ENGINE_ARG4;
  node = node->child;

  if (node != NULL)
    {      
      ENGINE_Term Answer = get_trie_entry(Arg,node);
      *(TagToPointer(ENGINE_ARG6) + 1) = Answer;
      Arg->node->term[3] = (ENGINE_Term) node;
      return TRUE;
    } 

  pop_choicept(Arg);
  return FALSE;
}

int set_diff_answer_trie(struct EACH_CALL *call, ENGINE_Term Ans)
{
  TrNode node;
  node=put_trie_entry(call->trie_answers,Ans);

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
  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG2);

  if (set_diff_answer_trie(callid, ENGINE_ARG1)) 
    {
      insert_answer(Arg, callid, ENGINE_ARG1);
    }
  
  return FALSE;  
}

bool_t consume_answer_c(Arg)
     Argdecl;
{
  struct EACH_CALL *callid=(struct EACH_CALL *) (ENGINE_IntOfTerm(ENGINE_ARG2));
  
  if (callid->comp != COMPLETE) return FALSE; //lanzar continuaciones

  if(callid->node_first==NULL) return FALSE;

  TrNode node = callid->node_first;
 
  push_choicept(Arg,address_nd_consume_answer_c);

  Arg->node->term[1] = (ENGINE_Term)node;

  if(ENGINE_Unify(ENGINE_ARG1,get_trie_entry(Arg,node)))
    {
      /*
	ENGINE_Term * term = (ENGINE_Term*) &(Arg->misc->goal_desc_ptr->worker_registers->node->term[1]);
	*term = (ENGINE_Term) ((int)id_solution - MallocBase);
	*term = (ENGINE_Term) ((int)*term << 2);
	*term = (ENGINE_Term) ((int)*term | 0x80000000);
	*/
      return TRUE;
    }

  //printf("\nNO ACTUALIZA\n"); return TRUE;
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
  
  node = (TrNode) ENGINE_ARG2;

  node=node->child;

  if(node==NULL) 
    {
      pop_choicept(Arg);
      return FALSE;
    }

  Arg->node->term[1] = (ENGINE_Term)node;

  if(ENGINE_Unify(ENGINE_ARG1,get_trie_entry(Arg,node))) 
    {
      return TRUE;
    }
  
  return FALSE;  

}

bool_t initial_c(Arg)
     Argdecl;
{
  ini = NULL;
  paux = NULL;
  memory_free = &memory[0];
  node_top = NULL;
  address_nd_consume_answer_c = def_retry_c(nd_consume_answer_c,2);
  address_nd_resume_ccalls_c = def_retry_c(nd_resume_ccalls_c,5);
  address_nd_new_ccall_c = def_retry_c(nd_new_ccall_c,6);
  init_tries_module();
  return TRUE;
}
