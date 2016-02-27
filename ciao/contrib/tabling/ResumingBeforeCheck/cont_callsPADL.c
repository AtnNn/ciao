/* -------------------------- */
/*          Includes          */
/* -------------------------- */

#include "cont_calls.h"
#include "tries.c"

/* -------------------------- */
/*     Inline Procedures      */
/* -------------------------- */

tagged_t *ini = NULL;
struct EACH_CALL *num1 = NULL;
ENGINE_Term *paux = NULL;
ENGINE_Term array[TERM_STACK_SIZE];

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
  ciao_ensure_heap(state, 4);  //change to in my_heap
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
  
  TrNode node = put_trie_entry(node_top,Call);
  if (num1 == NULL) num1 = (struct EACH_CALL *)node->child;

  if (node->child == NULL) 
    {
      node->child = (trie_node_t *)mem_alloc(sizeof(struct EACH_CALL));
      callid = (struct EACH_CALL *)(node->child);
      
      callid->answers = 0xc0000000 + ((unsigned int)tabled_top - MallocBase);
      HeapPush(tabled_top, (unsigned int)tabled_top - MallocBase);
      HeapPush(tabled_top, (unsigned int)tabled_top - MallocBase);
      callid->last_answer = *(tabled_top - 1);

      callid->conts = ENGINE_MkAtomTerm("[]");

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
  //else SETMIN(parentcallid->plink,call->plink);

  return TRUE;
}

void make_ground(Argdecl, ENGINE_Term *index, ENGINE_Term *indexFin)
{
  //printf("\nENTRA\n"); fflush(stdout);
  ENGINE_Term *i_term;
  int i, arity;
  ENGINE_Term *ptIni = tabled_top;
  ENGINE_Term *indexIni = index;
  ENGINE_Term *pt = tabled_top;
  int iarray = 0;

  //Puede haber variables tabling --> enlaces indeseados!!
  //Copiar el codigo de tries, que ahi se hace bien
 
  while(index < tabled_top)
    {
      //printf("\nWHILE index %p = %p\n",index,*index); fflush(stdout);
      ENGINE_Term t = *index;
      if ((t == 0xb0000008) || (t == 0x9000000c))
	{
	  //printf("\nENTRA EN 1\n"); fflush(stdout);
	  if (t == 0xb0000008) index = index + 3;
	  else index = index + 4;
	}
      else if (IsVar(t) || IsTrieVar(t))
	{
	  //printf("\nENTRA EN 2 t = %p\n",t); fflush(stdout);
	  DEREF(t,t);
	  //printf("\nPASA DEREF t = %p\n",t); fflush(stdout);
	  //printf("\nPASA DEREF\n"); fflush(stdout);
	  //printf("\nENTRA EN 3\n"); fflush(stdout);
	  if (IsTrieVar(t))  
	    {
	      //printf("\nENTRA EN 4\n"); fflush(stdout);
	      //printf("\nTrieVarindex = %d\n",TrieVarIndex(t));
	      HeapPush(index, array[TrieVarIndex(t)]);
	      //printf("\nApuntaba a %p donde tabled_top = %p\n",*((ENGINE_Term*)(array[TrieVarIndex(t)])),tabled_top); 
	      fflush(stdout);
	    }
	  else if (IsVar(t)) 
	    {
	      //printf("\nENTRA EN 5\n"); fflush(stdout);
	      if (iarray + 2 >= TERM_STACK_SIZE)
		fprintf(stderr, "\ncont_call module: TERM_STACK full");
	      
	      array[iarray++] = (unsigned int)index - MallocBase;
	      //printf("\n METO LA VARIABLE %p\n",(unsigned int)index - MallocBase);
	      HeapPush(index, (unsigned int)index - MallocBase);
	      array[iarray] = t;
	      CTagToPointer(t) = ENGINE_VarTrie | ((iarray-1) << 1);
	      //printf("\nMeto Valor %p\n",ENGINE_VarTrie | ((iarray-1) << 1));  
	      iarray++;
	      //printf("\nSALE DE 5\n"); fflush(stdout);
	    }
	  else *index = t;	  
	}
      else if (TagIsLST(t))
	{
	  //printf("\nENTRA EN 6\n"); fflush(stdout);
	  HeapPush(index, 0xc0000000 + ((unsigned int)tabled_top - MallocBase));
	  ENGINE_Term *list = TagToPointer(t);
	  HeapPush(tabled_top, *list);
	  list++;
	  HeapPush(tabled_top, *list);
	}
      else if (TagIsSTR(t))
	{
	  //printf("\nENTRA EN 7\n"); fflush(stdout);
	  if (IsNumber(t))
	    {
	      //printf("\nENTRA EN 8\n"); fflush(stdout);
	     if (TagIsSmall(t)) index++; 
	     else
	       {
		 //printf("\nENTRA EN 9\n"); fflush(stdout);
		 if (!LargeIsFloat(t)) //large integer
		   {
		     //printf("\nENTRA EN 10\n"); fflush(stdout);
		     HeapPush(index, 0xe0000000 + ((unsigned int)tabled_top - MallocBase));
		     HeapPush(tabled_top, 0xb0000008);
		     HeapPush(tabled_top, *(TagToPointer(t) + 1));
		     HeapPush(tabled_top, 0xb0000008);
		   }
		 else //float
		   {
		     //printf("\nENTRA EN 11\n"); fflush(stdout);
		     HeapPush(index, 0xe0000000 + ((unsigned int)tabled_top - MallocBase));
		     HeapPush(tabled_top, 0x9000000c);
		     HeapPush(tabled_top, *(TagToPointer(t) + 1));
		     HeapPush(tabled_top, *(TagToPointer(t) + 2));
		     HeapPush(tabled_top, 0x9000000c);
		   }
	       }
	    }
	  else
	    {
	      //printf("\nENTRA EN 12\n"); fflush(stdout);
	      HeapPush(index, 0xe0000000 + ((unsigned int)tabled_top - MallocBase));
	      i_term = TagToPointer(t);
	      arity = Arity(TagToHeadfunctor(t));

	      for (i = 0; i <= arity; i++)
		{
		  //printf("\nFOR i %p - arity %p\n",i,arity); fflush(stdout);
		  HeapPush(tabled_top, *i_term);
		  i_term++;
		}
	    }
	}
      else index++;

      //printf("\nENTRA EN 13\n"); fflush(stdout);
      if (index == indexFin) index = ptIni;
      //printf("\nANTES NUEVO WHILE\n"); fflush(stdout);
    }

  //printf("\nBUCLE\n"); fflush(stdout);
  //Reinstall original values of variables.
  int j;
  for(j = 1; j < iarray; j = j + 2) 
    {
      //printf("\nj = %d - iarray = %d\n",j,iarray);
      CTagToPointer(array[j]) = array[j];
    }
  //printf("\nSALE\n"); fflush(stdout);
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
  //printf("\nLimpiando tablas\n"); fflush(stdout);
  if (ini == NULL) return TRUE;
  //printf("\nTabling = %d - Freezing = %d\n", (unsigned long int)memory_free - 
  // (unsigned long int)&memory[0], (unsigned long int)tabled_top - (unsigned long int)Arg->heap_start);

  memory_free = &memory[0];
  tabled_top = ini;
  node_top = NULL;
  return TRUE;
}

bool_t tabled_call_c(Arg)
     Argdecl;
{
  //printf("\nEntra en tabled call\n"); fflush(stdout);

  if (ini == NULL) ini = tabled_top;
  
  if ((TOP_SF != NULL) && (ENGINE_IsVarTerm(ENGINE_ARG5)))
    {
      //printf("\nCalling Tabling->Prolog->Tabling\n");
    }
  

  if (node_top == NULL) node_top = open_trie();

  struct EACH_CALL *callid = handlecall(ENGINE_ARG1); 

  ENGINE_Unify(ENGINE_ARG2,MakeInteger(Arg, (unsigned int)callid));
  DEREF(ENGINE_ARG2,ENGINE_ARG2);

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
  //printf("\nEntra en resume_ccalls\n"); fflush(stdout);
  //printf("\ntabled_top = %p y heap = %p\n", tabled_top, Arg->global_top);
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
	  //printf("\nresume_ccalls que genera ND\n"); fflush(stdout);
	  ENGINE_Term F = ENGINE_HeadOfTerm(sf->conts);
	  *(TagToPointer(F) + 1) = ENGINE_HeadOfTerm(Answers);
	  DEREF(ENGINE_ARG2,ENGINE_ARG2);
	  push_choicept(Arg,address_nd_resume_ccalls_c);
	  ENGINE_Unify(ENGINE_ARG2, F);
	  //printf("\nresume_ccalls que genera ND node = %p coge cabecera de %p y sf%p\n",Arg->node,Answers,sf); fflush(stdout);
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
  //printf("\nEntra en resume calls ND con node %p\n",Arg->node); fflush(stdout);
  struct EACH_CALL *sf = (struct EACH_CALL *)ENGINE_ARG3;
  ENGINE_Term F;
  ENGINE_Term Answers = ENGINE_TailOfTerm(ENGINE_ARG5);

  if (Answers != sf->last_answer)
    {      
      //printf("\nLee nueva respuesta\n"); fflush(stdout);
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
      //printf("\nLee nueva continuacion\n"); fflush(stdout);
      Answers = ENGINE_TailOfTerm(ENGINE_HeadOfTerm(ENGINE_TailOfTerm(F)));
      if (ENGINE_HeadOfTerm(Answers) != sf->last_answer)
	{
	  Arg->node->term[3] = F;
	  Arg->node->term[4] = Answers;
	  F = ENGINE_HeadOfTerm(F);
	  *(TagToPointer(F) + 1) = ENGINE_HeadOfTerm(Answers);
	  //	  CTagToPointer(ENGINE_ARG2) = F;
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
	  //printf("\nLee nuevo generador\n"); fflush(stdout);
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
	  //printf("\nresume_ccalls que genera ND\n"); fflush(stdout);
	  F = ENGINE_HeadOfTerm(sf->conts);
	  *(TagToPointer(F) + 1) = ENGINE_HeadOfTerm(Answers);
	  ENGINE_Unify(ENGINE_ARG2, F);
	  //push_choicept(Arg,address_nd_resume_ccalls_c);
	  //printf("\nresume_ccalls que genera ND node = %p coge answer de %p y sf %p\n",Arg->node,Answers,sf); fflush(stdout);
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

void insert_cont(Argdecl, ENGINE_Term ContCall)
{
  make_ground(Arg, TagToPointer(ContCall), TagToPointer(ContCall) + 4);
}

bool_t freeze_c(Arg)
     Argdecl;
{
      HeapPush(tabled_top, SetArity(MakeString(ENGINE_AtomName(ENGINE_ARG1)), 3));
      HeapPush(tabled_top, (unsigned int)tabled_top - MallocBase);
      HeapPush(tabled_top, ENGINE_ARG2);
      HeapPush(tabled_top, ENGINE_ARG3);

      ENGINE_Term ContCall = Tag(STR, HeapOffset(tabled_top, -4));
      insert_cont(Arg, ContCall);
      //unificar con salida
      ENGINE_Unify(ENGINE_ARG4, ContCall);
      return TRUE;
}

//bool_t mostrar_c(Arg)
//     Argdecl;
//{
//  ENGINE_Term *aux;
//  for(aux = paux; aux <= paux + 4; aux++) 
//    {
//      printf("\n %p = %p y global = %p\n",aux, *aux, Arg->global_top); fflush(stdout);
//    }
//
//  //tagged_t *point;
//  //for (point = Heap_Start; point < tabled_top; point++) printf("\n%p = %p\n",point,*point);
//  return TRUE;
//}

bool_t new_ccall_c(Arg)
     Argdecl;
{
  //printf("\nEntra en new_call\n"); fflush(stdout);
  //  printf("\nEntra en new_ccall - tabled= %p, mitad = %p, ini = %p, actual = %p, final = %p\n",
  // tabled_top,
  // HeapOffset(Heap_Start, GLOBALSTKSIZE/2),
  // Arg->heap_start,
  // Arg->global_top,
  // Arg->heap_end); 
  //fflush(stdout);
  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG2);

  if (callid->comp==EVALUATING)
    {
      //printf("\nse esta evaluando\n"); fflush(stdout);
      struct EACH_CALL *parentcallid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG1);
      SETMIN(parentcallid->plink, callid->plink);  

      HeapPush(tabled_top, SetArity(MakeString(ENGINE_AtomName(ENGINE_ARG4)), 3));
      HeapPush(tabled_top, (unsigned int)tabled_top - MallocBase);
      HeapPush(tabled_top, ENGINE_ARG1);
      HeapPush(tabled_top, ENGINE_ARG3);

      //printf("\nEs repetida?\n");
      //TrNode node = put_trie_entry(callid->trie_cont,Tag(STR, HeapOffset(tabled_top, -4)));
      //if (node->child != NULL) return FALSE;
      //node->child = (TrNode)1;
      
      //printf("\nNO\n");
      insert_cont_call(Arg, callid, Tag(STR, HeapOffset(tabled_top, -4)));
      return FALSE;
    }

  //else COMPLETE
  //Return continuation consuming answers

  ENGINE_Term Answers = ENGINE_TailOfTerm(callid->answers);
  if (Answers == callid->last_answer) return FALSE;

  //printf("\nVITAL!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1 %p-%p\n",Answers,TagToPointer(Answers));

  //ENGINE_Unify(ENGINE_ARG6, ENGINE_HeadOfTerm(Answers));
  //ENGINE_Unify(ENGINE_ARG7, ENGINE_ARG3);
  ENGINE_Term *pt = Arg->global_top;
  paux = Arg->global_top;
  HeapPush(pt, SetArity(MakeString(ENGINE_AtomName(ENGINE_ARG4)), 3));
  HeapPush(pt, ENGINE_HeadOfTerm(Answers));
  HeapPush(pt, ENGINE_ARG1);
  HeapPush(pt, ENGINE_ARG3);
  Arg->global_top = pt;
  //ENGINE_Term *aux;
  //for(aux = paux; aux <= pt; aux++) 
  //  {
  //    printf("\n %p = %p y global = %p\n",aux, *aux, Arg->global_top); fflush(stdout);
  //  }
  ENGINE_Unify(ENGINE_ARG5, Tag(STR, HeapOffset(pt, -4)));
  push_choicept(Arg,address_nd_new_ccall_c);
  DEREF(ENGINE_ARG5,ENGINE_ARG5);
  //printf("\nF = %p\n",ENGINE_ARG5);
  Arg->node->term[3] = Answers;
  Arg->node->term[4] = ENGINE_ARG5;
  return TRUE;
}
bool_t nd_new_ccall_c(Arg)
     Argdecl;
{
  //printf("\nEntra en new_ccall ND\n"); fflush(stdout);
  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG2);
  //DEREF(ENGINE_ARG4,ENGINE_ARG4);
  ENGINE_ARG4 = ENGINE_TailOfTerm(ENGINE_ARG4);

  //printf("\n%p - %p\n",ENGINE_ARG4,callid->last_answer);

  if (ENGINE_ARG4 != callid->last_answer)
    {      
      *(TagToPointer(ENGINE_ARG5) + 1) = ENGINE_HeadOfTerm(ENGINE_ARG4);
      Arg->node->term[3] = ENGINE_ARG4;
      return TRUE;
    } 

  pop_choicept(Arg);
  return FALSE;
}

int set_diff_answer_trie(struct EACH_CALL *call, ENGINE_Term Ans)
{
  
  TrNode node;
  //printf("\nantes put %p - call->trie_answers\n"); fflush(stdout);
  node=put_trie_entry(call->trie_answers,Ans);
  //printf("\ntras put\n"); fflush(stdout);

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
  //printf("\nEntra en new answer\n"); fflush(stdout);
  struct EACH_CALL *callid = (struct EACH_CALL *)ENGINE_IntOfTerm(ENGINE_ARG2);
  //printf("\nnew answer 1 %p - %p\n",callid,ENGINE_ARG1); fflush(stdout);

 if (set_diff_answer_trie(callid, ENGINE_ARG1)) 
    {
      //printf("\nnew answer 2\n"); fflush(stdout);
      //printf("\nInsertada\n");
      insert_answer(Arg, callid,ENGINE_ARG1);
      //printf("\nnew answer 3\n"); fflush(stdout);
    }
 //printf("\nSale de new answer\n"); fflush(stdout);
  return FALSE;  
}

bool_t consume_answer_c(Arg)
     Argdecl;
{
  //printf("\nEntra en consume answer\n"); fflush(stdout);
  struct EACH_CALL *callid=(struct EACH_CALL *) (ENGINE_IntOfTerm(ENGINE_ARG2));
  
  if (callid->comp != COMPLETE) return FALSE; //lanzar continuaciones

  if(callid->node_first==NULL) return FALSE;

  TrNode node = callid->node_first;

  if (node==NULL) return FALSE;

  push_choicept(Arg,address_nd_consume_answer_c);

  if(ENGINE_Unify(ENGINE_ARG1,get_trie_entry(Arg,callid->node_first)))
    {
      Arg->node->term[1] = (ENGINE_Term)node;
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
  //printf("\nEntra en consume answer ND\n"); fflush(stdout);
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

  if(ENGINE_Unify(ENGINE_ARG1,get_trie_entry(Arg,node))) return TRUE;
  
  //pop_choicept(Arg);
  return FALSE;  

}

bool_t initial_c(Arg)
     Argdecl;
{
  address_nd_consume_answer_c = def_retry_c(nd_consume_answer_c,2);
  address_nd_resume_ccalls_c = def_retry_c(nd_resume_ccalls_c,5);
  address_nd_new_ccall_c = def_retry_c(nd_new_ccall_c,5);
  init_tries_module();
  return TRUE;
}
